open Lwt.Syntax
open Sexplib0.Sexp_conv

module type CON = sig
  val conduit : Conduit_mirage.t
end

module Make
    (T : Mirage_time.S)
    (P : Plog.S)
    (Server : Cohttp_lwt.S.Server)
    (C : CON) =
struct
  type plog_entry = P.entry [@@deriving sexp]

  type address = Uri_sexp.t [@@deriving sexp]

  type args = {
    term : int;
    leader_id : int;
    prev_log_index : int;
    prev_log_term : int;
    entries : plog_entry list;
    leader_commit : int;
  }
  [@@deriving make, sexp]

  type res = { id : int; term : int; success : bool } [@@deriving make, sexp]

  let res_push = ref None

  let send uri args =
    let body =
      sexp_of_args args |> Sexplib0.Sexp.to_string |> Cohttp_lwt.Body.of_string
    in
    let* request =
      let ctx = Cohttp_mirage.Client.ctx Resolver_mirage.localhost C.conduit in
      let uri = Uri.with_path uri "/append_entries" in
      Lwt.catch
        (fun () ->
          let* res, body = Cohttp_mirage.Client.post ~ctx ~body uri in
          Lwt.return_some (res, body))
        (fun exn ->
          let+ () =
            match exn with
            (* | Failure s -> *)
            (* Logs_lwt.debug (fun f -> *)
            (*     f "Failed to send append entries to %s: %s" *)
            (*       (Uri.to_string uri) s) *)
            | _ ->
                (* Logs_lwt.debug (fun f -> *)
                (* f "Failed to send append entries to %s" (Uri.to_string uri)) *)
                Lwt.return_unit
          in
          None)
    in
    match request with
    | None -> Lwt.return_unit
    | Some (res, body) -> (
        match Cohttp.Response.status res with
        | `OK -> (
            let* body = Cohttp_lwt.Body.to_string body in
            let res = Sexplib.Sexp.of_string body |> res_of_sexp in
            Lwt.return
            @@
            match !res_push with None -> () | Some p -> p (Some (args, res)) )
        | _ ->
            (* Logs_lwt.debug (fun f -> f "error from call to append entries") *)
            Lwt.return_unit )

  let handle_request ae_args_push body =
    let* body = Cohttp_lwt.Body.to_string body in
    let args = Sexplib.Sexp.of_string body |> args_of_sexp in
    let mvar = Lwt_mvar.create_empty () in
    ae_args_push (Some (args, mvar));
    let* response = Lwt_mvar.take mvar in
    let body = sexp_of_res response |> Sexplib0.Sexp.to_string in
    Server.respond_string ~status:`OK ~body ()
end
