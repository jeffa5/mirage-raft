open Sexplib.Std

module type S = sig
  include Mirage_raft.Plog.S
end

module Make (C : Mirage_raft.Command.S) : S with type command = C.input = struct
  type command = C.input [@@deriving sexp]

  type entry = { term : int; command : command } [@@deriving make, sexp]

  type item = int * entry [@@deriving sexp]

  type t = { current_term : int; voted_for : int option; items : item list }
  [@@deriving sexp]

  let v () = Lwt.return { current_term = 0; voted_for = None; items = [] }

  let current_term t = Lwt.return t.current_term

  let voted_for t = Lwt.return t.voted_for

  let set_current_term c t = Lwt.return { t with current_term = c }

  let set_voted_for v t = Lwt.return { t with voted_for = v }

  let insert i e t =
    let t = { t with items = (i, e) :: t.items } in
    Lwt.return t

  let get i t =
    List.fold_left
      (fun acc (index, e) -> if i = index then Some e else acc)
      None t.items
    |> Lwt.return

  let delete_from i t =
    let items =
      List.filter_map
        (fun (index, e) -> if index >= i then None else Some (index, e))
        t.items
    in
    Lwt.return { t with items }

  let get_from i t =
    List.filter_map
      (fun (index, e) -> if index >= i then Some e else None)
      t.items
    |> Lwt.return

  let last_entry t =
    match t.items with
    | [] -> Lwt.return_none
    | _ ->
        let i, term =
          List.fold_left
            (fun (ci, term) (i, e) ->
              if i > ci then (i, e.term) else (ci, term))
            (-1, -1) t.items
        in
        Lwt.return_some (i, term)
end
