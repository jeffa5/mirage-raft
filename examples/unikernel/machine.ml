open Sexplib0.Sexp_conv
open Asetmap
open Lwt.Syntax
module StringMap = Map.Make (String)

module Make (Server : Cohttp_lwt.S.Server) = struct
  type t = string StringMap.t

  let t_of_sexp s = [%of_sexp: (string * string) list] s |> StringMap.of_list

  let sexp_of_t t = StringMap.to_list t |> [%sexp_of: (string * string) list]

  type input = Set of string * string | Get of string [@@deriving sexp]

  type output = Added | Value of string option [@@deriving sexp]

  let t = ref StringMap.empty

  let apply (input : input) =
    match input with
    | Set (i, s) ->
        let map = StringMap.add i s !t in
        t := map;
        Added
    | Get i ->
        let item = StringMap.find_opt i !t in
        Value item

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
            push_command (Some (Get key, mvar));
            let* response = Lwt_mvar.take mvar in
            match response with
            | None -> Server.respond_not_found ()
            | Some i ->
                let output = apply i in
                let body = sexp_of_output output |> Sexplib0.Sexp.to_string in
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
            push_command (Some (Set (key, body), mvar));
            let* response = Lwt_mvar.take mvar in
            match response with
            | None -> Server.respond_not_found ()
            | Some i ->
                let output = apply i in
                let body = sexp_of_output output |> Sexplib0.Sexp.to_string in
                Server.respond_string ~status:`OK ~body () ) )
    | _ -> Server.respond_not_found ()
end
