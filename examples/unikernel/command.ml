open Sexplib0.Sexp_conv
open Asetmap
open Lwt.Syntax
module StringMap = Map.Make (String)

module Make (Server : Cohttp_lwt.S.Server) = struct
  type machine = string StringMap.t

  let machine_of_sexp s =
    [%of_sexp: (string * string) list] s |> StringMap.of_list

  let sexp_of_machine t =
    StringMap.to_list t |> [%sexp_of: (string * string) list]

  type machine_input = Set of string * string | Get of string
  [@@deriving sexp]

  type input =
    | MachineInput of machine_input
    | AddPeers of int list
    | RemovePeers of int list
  [@@deriving sexp]

  type output = Result of input | NotLeader of int option [@@deriving sexp]

  type machine_output = Added | Value of string option [@@deriving sexp]

  let t = ref StringMap.empty

  let apply = function
    | MachineInput (Set (i, s)) ->
        let map = StringMap.add i s !t in
        t := map;
        Added
    | MachineInput (Get i) ->
        let item = StringMap.find_opt i !t in
        Value item
    | AddPeers _ -> Added
    | RemovePeers _ -> Added

  let handle_request push_command req body =
    let open Cohttp in
    let path = req |> Request.uri |> Uri.path in
    let* () = Logs_lwt.info (fun f -> f "path: %s" path) in
    let path = Astring.String.with_index_range ~first:9 path in
    let meth = req |> Request.meth |> Code.string_of_method in
    match meth with
    | "GET" -> (
        match path with
        | "" | "/" -> Server.respond_not_found ()
        | key -> (
            let* () =
              Logs_lwt.info (fun f -> f "Command received: %s %s" meth path)
            in
            let mvar = Lwt_mvar.create_empty () in
            push_command (Some (MachineInput (Get key), mvar));
            let* response = Lwt_mvar.take mvar in
            match response with
            | NotLeader None -> Server.respond_not_found ()
            | NotLeader (Some i) ->
                Server.respond_string ~status:`Not_found ~body:(string_of_int i)
                  ()
            | Result i ->
                let output = apply i in
                let body =
                  sexp_of_machine_output output |> Sexplib0.Sexp.to_string
                in
                Server.respond_string ~status:`OK ~body () ) )
    | "PUT" -> (
        match path with
        | "" | "/" -> Server.respond_not_found ()
        | key -> (
            let* body = Cohttp_lwt.Body.to_string body in
            let* () =
              Logs_lwt.info (fun f ->
                  f "Command received: %s %s %s" meth key body)
            in
            let mvar = Lwt_mvar.create_empty () in
            push_command (Some (MachineInput (Set (key, body)), mvar));
            let* response = Lwt_mvar.take mvar in
            match response with
            | NotLeader None -> Server.respond_not_found ()
            | NotLeader (Some i) ->
                Server.respond_string ~status:`Not_found ~body:(string_of_int i)
                  ()
            | Result i ->
                let output = apply i in
                let body =
                  sexp_of_machine_output output |> Sexplib0.Sexp.to_string
                in
                Server.respond_string ~status:`OK ~body () ) )
    | _ -> Server.respond_not_found ()
end
