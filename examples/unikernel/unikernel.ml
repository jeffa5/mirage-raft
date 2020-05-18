open Lwt.Syntax

module Raft
    (S : Mirage_stack.V4)
    (Conduit : Conduit_mirage.S)
    (T : Mirage_time.S)
    (Rand : Mirage_random.S)
    (PClock : Mirage_clock.PCLOCK) =
struct
  module Logs_reporter = Mirage_logs.Make (PClock)
  module Server = Cohttp_mirage.Server (S.TCPV4)

  let rec zip = function
    | _, [] -> []
    | [], _ -> []
    | x :: xs, y :: ys -> (x, y) :: zip (xs, ys)

  let start s conduit _t _r _p =
    let module Con = struct
      let conduit = conduit
    end in
    let module M = Machine.Make (Server) in
    let module P = Plog.Make (M) in
    let module Ae = Append_entries.Make (T) (P) (Server) (Con) in
    let module Rv = Request_votes.Make (Server) (Con) in
    let module R = Mirage_raft.Raft.Make (T) (Rand) (M) (P) (Ae) (Rv) in
    let server ae_req_push rv_req_push commands_push =
      let open Cohttp in
      let callback _conn req body =
        let path = req |> Request.uri |> Uri.path in
        match path with
        | "/append_entries" -> Ae.handle_request ae_req_push body
        | "/request_votes" -> Rv.handle_request rv_req_push body
        | _ ->
            if Astring.String.is_prefix ~affix:"/machine" path then
              M.handle_request commands_push req body
            else Server.respond_not_found ()
      in
      Server.listen (Server.make ~callback ())
    in
    let port = Key_gen.port () in
    let id = Key_gen.id () in
    let peer_ids = Key_gen.peer_ids () in
    let peer_uris = Key_gen.peer_uris () |> List.map Uri.of_string in
    let peers = zip (peer_ids, peer_uris) in
    let dbg = Key_gen.debug () in

    let () = Logs.(set_level (Some (if dbg then Debug else Info))) in
    let ae_req, ae_req_push = Lwt_stream.create () in
    let ae_res, ae_res_push = Lwt_stream.create () in
    Ae.res_push := Some ae_res_push;
    let rv_req, rv_req_push = Lwt_stream.create () in
    let rv_res, rv_res_push = Lwt_stream.create () in
    Rv.res_push := Some rv_res_push;
    let commands, commands_push = Lwt_stream.create () in
    S.listen_tcpv4 s ~port (server ae_req_push rv_req_push commands_push);
    let* () = Logs_lwt.info (fun f -> f "Started listening on port %d" port) in
    Logs_reporter.(create () |> run) @@ fun () ->
    let () = Lwt.async (fun () -> S.listen s) in
    let* raft =
      R.v ~timeout_lower:(Key_gen.timeout_lower ())
        ~timeout_upper:(Key_gen.timeout_upper ())
        ~heartbeat:(Key_gen.heartbeat ()) ae_req ae_res rv_req rv_res commands
        id peers
    in
    R.handle raft
end
