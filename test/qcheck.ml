open Sexplib0.Sexp_conv
open Asetmap
open Lwt.Syntax

module P = struct
  type entry = { term : int; command : string } [@@deriving make, sexp]

  type item = int * entry [@@deriving sexp]

  type t = { current_term : int; voted_for : int option; items : item list }
  [@@deriving sexp]

  let v () = Lwt.return { current_term = 0; voted_for = None; items = [] }

  let current_term t = Lwt.return t.current_term

  let voted_for t = Lwt.return t.voted_for

  let set_current_term t c = Lwt.return { t with current_term = c }

  let set_voted_for t v = Lwt.return { t with voted_for = v }

  let insert t i e =
    let t = { t with items = (i, e) :: t.items } in
    Lwt.return t

  let get t i =
    List.fold_left
      (fun acc (index, e) -> if i = index then Some e else acc)
      None t.items
    |> Lwt.return

  let delete_from t i =
    let items =
      List.filter_map
        (fun (index, e) -> if index >= i then None else Some (index, e))
        t.items
    in
    Lwt.return { t with items }

  let get_from t i =
    List.filter_map
      (fun (index, e) -> if index >= i then Some e else None)
      t.items
    |> Lwt.return
end

module Ae = struct
  type plog_entry = P.entry [@@deriving sexp]

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

  let send _uri _args = Lwt.return_unit
end

module Rv = struct
  type args = {
    term : int;
    candidate_id : int;
    last_log_index : int;
    last_log_term : int;
  }
  [@@deriving make, sexp]

  type res = { id : int; term : int; vote_granted : bool }
  [@@deriving make, sexp]

  let send _uri _args = Lwt.return_unit
end

module M = struct
  type t = int [@@deriving sexp]

  type input = Ae.plog_entry [@@deriving sexp]

  type output = int [@@deriving sexp]

  let v () = 0

  let apply t (input : input) = (t + input.term, input.term)
end

module Ev = Mirage_raft.Event.Make (Ae) (Rv) (M)
module Ac = Mirage_raft.Action.Make (Ae) (Rv) (M)
module State = Mirage_raft.State.Make (P) (Ae) (Rv) (M) (Ev) (Ac)

let plog_entry =
  QCheck.(
    pair small_nat small_string
    |> map (fun (term, command) -> P.make_entry ~term ~command))

type input = Event of int * Ev.t | Rpc of int | RestartServer of int
[@@deriving sexp]

let input =
  QCheck.(
    oneof
      [
        pair (0 -- 2)
          (oneof
             [
               always Ev.ElectionTimeout;
               always Ev.SendHeartbeat;
               plog_entry
               |> map (fun i ->
                      Ev.CommandReceived (i, Lwt_mvar.create_empty ()));
             ])
        |> map (fun (i, e) -> Event (i, e));
        small_nat |> map (fun i -> Rpc i);
        map (fun i -> RestartServer i) (0 -- 2);
      ])

let new_actions ((i, e) : int * Ev.t) servers =
  let* servers_actions =
    Lwt_list.mapi_s
      (fun index s ->
        if index = i then State.handle s e else Lwt.return (s, []))
      servers
  in
  let* servers = Lwt_list.map_s (fun (s, _) -> Lwt.return s) servers_actions in
  let+ actions =
    Lwt_list.fold_left_s
      (fun acc (_, l) -> Lwt.return (acc @ l))
      [] servers_actions
  in
  let new_events =
    List.filter_map
      (function
        | Ac.ResetElectionTimeout -> None
        | Ac.AppendEntriesRequest (i, args) ->
            Some (i, Ev.AppendEntriesRequest (args, Lwt_mvar.create_empty ()))
        | Ac.AppendEntriesResponse (res, _) ->
            Some
              ( res.id,
                Ev.AppendEntriesResponse
                  ( ( match e with
                    | Ev.AppendEntriesRequest (args, _) -> args
                    | _ -> assert false ),
                    res ) )
        | Ac.RequestVotesRequest (i, args) ->
            Some (i, Ev.RequestVotesRequest (args, Lwt_mvar.create_empty ()))
        | Ac.RequestVotesResponse (res, _) ->
            Some (res.id, Ev.RequestVotesResponse res)
        | Ac.CommandResponse _ -> None)
      actions
  in
  (servers, new_events)

let inputs =
  let open QCheck in
  list input
  |> set_print
       Print.(
         list
           (comap
              (fun p -> [%sexp_of: input] p |> Sexplib0.Sexp.to_string_hum)
              string))
  |> set_shrink Shrink.(list ?shrink:input.shrink)
  |> set_stats [ ("length", fun l -> List.length l) ]

let actions_to_rpcs e l =
  List.filter_map
    (function
      | Ac.AppendEntriesRequest (i, args) ->
          Some (i, Ev.AppendEntriesRequest (args, Lwt_mvar.create_empty ()))
      | Ac.AppendEntriesResponse (res, _) ->
          Some
            ( res.id,
              Ev.AppendEntriesResponse
                ( ( match e with
                  | Ev.AppendEntriesRequest (args, _) -> args
                  | _ -> assert false ),
                  res ) )
      | Ac.RequestVotesRequest (i, args) ->
          Some (i, Ev.RequestVotesRequest (args, Lwt_mvar.create_empty ()))
      | Ac.RequestVotesResponse (res, _) ->
          Some (res.id, Ev.RequestVotesResponse res)
      | Ac.CommandResponse _ -> None
      | Ac.ResetElectionTimeout -> None)
    l

let states =
  QCheck.(
    inputs
    |> map
         ~rev:(fun states ->
           Lwt_main.run states |> List.filter_map (fun (i, _) -> i))
         (fun inputs ->
           let server id p1id p2id =
             let+ log = P.v () in
             let peers =
               [
                 State.make_peer ~id:p1id ~address:Uri.empty ();
                 State.make_peer ~id:p2id ~address:Uri.empty ();
               ]
             in
             State.make ~id ~peers ~log ()
           in
           let* servers =
             [ server 0 1 2; server 1 0 2; server 2 0 1 ]
             |> Lwt_list.map_s (fun i -> i)
           in
           let rec last = function
             | [] -> assert false
             | [ x ] -> x
             | _ :: xs -> last xs
           in

           let+ states, _ =
             Lwt_list.fold_left_s
               (fun ((states : (input option * State.t list) list), rpcs) input ->
                 let servers =
                   let _, s = last states in
                   s
                 in
                 match input with
                 | RestartServer i ->
                     Lwt.return
                       ( ( Some input,
                           List.map
                             (fun (s : State.t) ->
                               if s.id = i then
                                 let s =
                                   State.make ~id:s.id ~peers:s.peers ~log:s.log
                                     ()
                                 in
                                 s
                               else s)
                             servers )
                         :: states,
                         rpcs )
                 | Event (i, event) ->
                     let+ servers, rs =
                       Lwt_list.fold_left_s
                         (fun (servers, rpcs) (s : State.t) ->
                           if s.id = i then
                             let+ s, acs = State.handle s event in
                             let rs = actions_to_rpcs event acs in
                             (s :: servers, rpcs @ rs)
                           else Lwt.return (s :: servers, rpcs))
                         ([], []) (List.rev servers)
                     in
                     ((Some input, servers) :: states, rpcs @ rs)
                 | Rpc i -> (
                     let _, rpc, rest =
                       List.fold_left
                         (fun (index, o, acs) action ->
                           if index = i mod List.length rpcs then
                             (index + 1, Some action, acs)
                           else (index + 1, o, action :: acs))
                         (0, None, []) rpcs
                     in
                     match rpc with
                     | None -> Lwt.return ((Some input, servers) :: states, rpcs)
                     | Some (i, rpc) ->
                         let+ servers, rpcs =
                           Lwt_list.fold_left_s
                             (fun (servers, rpcs) (s : State.t) ->
                               if s.id = i then
                                 let+ s, acs = State.handle s rpc in
                                 let rs = actions_to_rpcs rpc acs in
                                 (servers @ [ s ], rpcs @ rs)
                               else Lwt.return (servers @ [ s ], rpcs))
                             ([], []) servers
                         in
                         ( (Some (Event (i, rpc)), servers) :: states,
                           List.rev rest @ rpcs ) ))
               ([ (None, servers) ], [])
               inputs
           in
           List.rev states)
    |> set_print
         Print.(
           comap
             (fun l ->
               Lwt_main.run l |> [%sexp_of: (input option * State.t list) list]
               |> Sexplib0.Sexp.to_string_hum)
             string))

let election_safety =
  QCheck.(
    Test.make ~count:10_000 ~name:"Election safety: at most one leader per term"
      states (fun states ->
        let module TermMap = Map.Make (Int) in
        let leaders = TermMap.empty in

        let check_leader (s : State.t) leaders =
          match s.stage with
          | State.Leader -> (
              let+ term = P.current_term s.log in
              match TermMap.find term leaders with
              | None -> (TermMap.add term s.id leaders, true)
              | Some i when i = s.id -> (leaders, true)
              | Some _ -> (leaders, false) )
          | _ -> Lwt.return (leaders, true)
        in

        let lwt =
          let* states = states in
          let+ _, ok =
            Lwt_list.fold_left_s
              (fun (leaders, ok) state ->
                let _, servers = state in
                let+ leaders, ok =
                  Lwt_list.fold_left_s
                    (fun (leaders, ok) s ->
                      if ok then
                        let+ leaders, ok = check_leader s leaders in
                        (leaders, ok)
                      else Lwt.return (leaders, false))
                    (leaders, ok) servers
                in
                (leaders, ok))
              (leaders, true) states
          in
          ok
        in
        Lwt_main.run lwt))

let tests = List.map QCheck_alcotest.to_alcotest [ election_safety ]
