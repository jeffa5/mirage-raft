open Sexplib0.Sexp_conv
open Lwt.Syntax
open Asetmap

module Make
    (C : Command.S)
    (P : Plog.S with type command := C.input)
    (Ae : Append_entries.S with type plog_entry := P.entry)
    (Rv : Request_vote.S)
    (Ev : Event.S
            with type ae_args := Ae.args
             and type ae_res := Ae.res
             and type rv_arg := Rv.args
             and type rv_res := Rv.res
             and type command_input := C.input
             and type command_output := C.output)
    (Ac : Action.S
            with type ae_args := Ae.args
             and type ae_res := Ae.res
             and type rv_arg := Rv.args
             and type rv_res := Rv.res
             and type command_output := C.output) =
struct
  module CommandMap = struct
    module Mp = Map.Make (Int)
    include Mp

    type t = C.output Lwt_mvar.t Mp.t

    let t_of_sexp s =
      [%of_sexp: (int * (C.output Lwt_mvar.t[@opaque])) list] s |> Mp.of_list

    let sexp_of_t t =
      Mp.to_list t |> [%sexp_of: (int * (C.output Lwt_mvar.t[@opaque])) list]
  end

  type stage = Leader | Candidate | Follower [@@deriving sexp]

  type peer = {
    id : int;  (** id of the peer *)
    voting : bool; [@default true]
        (** whether the peer is ready to vote, used during cluster membership changes *)
    next_index : int; [@default 0]
    match_index : int; [@default 0]
  }
  [@@deriving make, sexp]

  type t = {
    stage : stage; [@default Follower]
    id : int;
    peers : peer list;
    log : P.t;
    commit_index : int; [@default 0]
    last_applied : int; [@default 0]
    votes_received : int; [@default 0]
    replicating : CommandMap.t; [@default CommandMap.empty]
  }
  [@@deriving make, sexp]

  let get_last_entry log =
    let+ le = P.last_entry log in
    match le with None -> (0, 0) | Some (i, t) -> (i, t)

  let handle_send_heartbeat (t : t) =
    match t.stage with
    | Follower | Candidate -> Lwt.return (t, [])
    | Leader ->
        let* term = P.current_term t.log in
        let leader_id = t.id in
        let leader_commit = t.commit_index in
        let+ actions =
          Lwt_list.map_s
            (fun peer ->
              let* last_log_index, _ = get_last_entry t.log in
              let prev_log_index = peer.next_index - 1 in
              let* prev_log_term =
                let+ prev_entry = P.get prev_log_index t.log in
                match prev_entry with None -> -1 | Some e -> e.term
              in
              if last_log_index >= peer.next_index then
                let+ entries = P.get_from peer.next_index t.log in
                let args =
                  Ae.make_args ~term ~leader_id ~prev_log_index ~prev_log_term
                    ~entries ~leader_commit ()
                in
                Ac.AppendEntriesRequest (peer.id, args)
              else
                let args =
                  Ae.make_args ~term ~leader_id ~prev_log_index ~prev_log_term
                    ~leader_commit ()
                in
                Lwt.return (Ac.AppendEntriesRequest (peer.id, args)))
            t.peers
        in
        (* if there exists an N s.t. N > commit_index, a majority of match_index[i] >= N, and log[N].term == current_term  set commit_index = N *)
        (t, actions)

  let become_leader (t : t) =
    let* log = P.set_voted_for None t.log in
    let* peers =
      let+ last_log_index, _ = get_last_entry t.log in
      List.map
        (fun p -> { p with next_index = last_log_index + 1; match_index = 0 })
        t.peers
    in
    let t = { t with peers; stage = Leader; log; votes_received = 0 } in
    handle_send_heartbeat t

  let become_candidate (t : t) =
    (* increment current_term *)
    let* term = P.current_term t.log in
    let term = term + 1 in
    let* log = P.set_current_term term t.log in
    (* vote for self *)
    let* log = P.set_voted_for (Some t.id) log in
    let t = { t with log; stage = Candidate; votes_received = 1 } in
    (* send request_vote rpcs to all other servers *)
    let+ request_votes =
      let+ last_log_index, last_log_term = get_last_entry t.log in
      let rv_args =
        Rv.make_args ~term ~candidate_id:t.id ~last_log_index ~last_log_term
      in
      List.map (fun (p : peer) -> Ac.RequestVoteRequest (p.id, rv_args)) t.peers
    in
    (t, Ac.ResetElectionTimeout :: request_votes)

  let become_follower (t : t) term =
    let* log = P.set_current_term term t.log in
    let+ log = P.set_voted_for None log in
    ({ t with log; stage = Follower }, [ Ac.ResetElectionTimeout ])

  let majority t c =
    let voting_peers =
      List.fold_left (fun c p -> if p.voting then c + 1 else c) 0 t.peers
    in
    2 * c > voting_peers + 1

  let apply_entries_to_machine t =
    let* current_term = P.current_term t.log in
    let* entries_since_commit_index = P.get_from (t.commit_index + 1) t.log in
    (* if there exists an N s.t. N > commit_index, a majority of match_index[i] >= N, and log[N].term == current_term  set commit_index = N *)
    let commit_index =
      List.fold_left
        (fun ci ((i, entry) : int * P.entry) ->
          if entry.term == current_term then
            let match_count =
              List.fold_left
                (fun count peer ->
                  if peer.match_index >= i then count + 1 else count)
                1 t.peers
            in
            if majority t match_count then i else ci
          else ci)
        t.commit_index
        (List.mapi
           (fun i e -> (i + t.commit_index + 1, e))
           entries_since_commit_index)
    in
    if commit_index <> t.commit_index then
      let t = { t with commit_index } in
      if t.commit_index <= t.last_applied then Lwt.return (t, [])
      else
        let+ entries =
          List.init (t.commit_index - t.last_applied) (fun i ->
              i + t.last_applied + 1)
          |> Lwt_list.filter_map_s (fun i ->
                 let+ entry = P.get i t.log in
                 match entry with None -> None | Some e -> Some (i, e))
        in
        let t = { t with last_applied = t.commit_index } in
        List.fold_left
          (fun (t, actions) ((i, e) : int * P.entry) ->
            let command_mvar = CommandMap.find i t.replicating in
            match command_mvar with
            | None -> (t, actions)
            | Some mvar ->
                let replicating = CommandMap.remove i t.replicating in
                ( { t with replicating },
                  Ac.CommandResponse (Result e.command, mvar) :: actions ))
          (t, []) entries
    else Lwt.return (t, [])

  let handle_timeout (t : t) =
    match t.stage with
    | Leader -> Lwt.return (t, [])
    | Candidate -> become_candidate t
    | Follower -> (
        match t.peers with
        | [] ->
            let* current_term = P.current_term t.log in
            let* log = P.set_current_term (current_term + 1) t.log in
            become_leader { t with log }
        | _ -> become_candidate t )

  let handle_append_entries_request (t : t)
      ((req, mvar) : Ae.args * Ae.res Lwt_mvar.t) =
    let* t, actions =
      let* current_term = P.current_term t.log in
      if req.term > current_term then
        let+ t, a = become_follower t req.term in
        (t, a)
      else Lwt.return (t, [])
    in
    let* term = P.current_term t.log in
    if req.term < term then
      (* reply false if term < current_term *)
      let response = Ae.make_res ~id:t.id ~term ~success:false in
      Lwt.return
        ( t,
          actions
          @ [
              Ac.ResetElectionTimeout; Ac.AppendEntriesResponse (response, mvar);
            ] )
    else
      let add_entries () =
        let* t, _ =
          Lwt_list.fold_left_s
            (fun (t, i) (entry : P.entry) ->
              let* e = P.get i t.log in
              match e with
              | None -> Lwt.return (t, i + 1)
              | Some e ->
                  (* if an existing entry conflicts with a new one (same index but different terms), delete the existing entry and all that follow it *)
                  if e.term = entry.term then Lwt.return (t, i + 1)
                  else
                    let+ log = P.delete_from i t.log in
                    ({ t with log }, i + 1))
            (t, req.prev_log_index + 1)
            req.entries
        in
        let* t, _ =
          Lwt_list.fold_left_s
            (fun (t, i) entry ->
              (* append any new entries not already in the log *)
              let+ log = P.insert i entry t.log in
              ({ t with log }, i + 1))
            (t, req.prev_log_index + 1)
            req.entries
        in
        let response = Ae.make_res ~id:t.id ~term ~success:true in
        if req.leader_commit > t.commit_index then
          let t =
            {
              t with
              commit_index =
                min req.leader_commit
                  (req.prev_log_index + List.length req.entries);
            }
          in
          Lwt.return
            ( t,
              actions
              @ [
                  Ac.ResetElectionTimeout;
                  Ac.AppendEntriesResponse (response, mvar);
                ] )
        else
          Lwt.return
            ( t,
              actions
              @ [
                  Ac.ResetElectionTimeout;
                  Ac.AppendEntriesResponse (response, mvar);
                ] )
      in
      let* prev_log_index_entry = P.get req.prev_log_index t.log in
      (* reply false if log doesn't contain an entry at prev_log_index whose term matches prev_log_term *)
      match prev_log_index_entry with
      | None -> add_entries ()
      | Some entry ->
          if entry.term <> req.prev_log_term then
            let response = Ae.make_res ~id:t.id ~term ~success:false in
            Lwt.return
              ( t,
                actions
                @ [
                    Ac.ResetElectionTimeout;
                    Ac.AppendEntriesResponse (response, mvar);
                  ] )
          else add_entries ()

  let handle_append_entries_response (t : t) ((args, res) : Ae.args * Ae.res) =
    let* term = P.current_term t.log in
    if res.term > term then become_follower t res.term
    else
      match t.stage with
      | Follower | Candidate -> Lwt.return (t, [])
      | Leader ->
          if res.term <> term then Lwt.return (t, [])
          else
            let* t, peers, acs =
              Lwt_list.fold_left_s
                (fun (t, ps, acs) (p : peer) ->
                  if p.id <> res.id then Lwt.return (t, p :: ps, acs)
                  else if res.success then
                    (* update next_index and match_index for follower *)
                    let next_index = p.next_index + List.length args.entries in
                    let match_index = next_index - 1 in
                    let p = { p with next_index; match_index } in
                    Lwt.return (t, p :: ps, acs)
                  else
                    (* decrement next_index and retry *)
                    let next_index = max 0 (p.next_index - 1) in
                    let p = { p with next_index } in
                    let* entries = P.get_from next_index t.log in
                    let prev_log_index = max 0 (next_index - 1) in
                    let+ prev_entry = P.get prev_log_index t.log in
                    let prev_log_term =
                      match prev_entry with None -> -1 | Some e -> e.term
                    in
                    let args =
                      Ae.make_args ~term ~leader_id:t.id ~prev_log_index
                        ~prev_log_term ~entries ~leader_commit:t.commit_index ()
                    in
                    (t, p :: ps, Ac.AppendEntriesRequest (p.id, args) :: acs))
                (t, [], []) t.peers
            in
            let t = { t with peers } in
            let+ t, actions =
              if res.success then apply_entries_to_machine t
              else Lwt.return (t, [])
            in
            (t, acs @ actions)

  let handle_request_votes_request (t : t)
      ((req, mvar) : Rv.args * Rv.res Lwt_mvar.t) =
    let* t =
      let* term = P.current_term t.log in
      if req.term > term then
        let+ t, _ = become_follower t req.term in
        t
      else Lwt.return t
    in
    let* term = P.current_term t.log in
    (* reply false if term < current_term *)
    if req.term < term then
      let resp = Rv.make_res ~id:t.id ~term ~vote_granted:false in
      Lwt.return
        (t, [ Ac.ResetElectionTimeout; Ac.RequestVoteResponse (resp, mvar) ])
    else
      let* voted_for = P.voted_for t.log in
      let disallow_vote t =
        let resp = Rv.make_res ~id:t.id ~term ~vote_granted:false in
        Lwt.return
          (t, [ Ac.ResetElectionTimeout; Ac.RequestVoteResponse (resp, mvar) ])
      in
      let allow_vote t =
        let+ t =
          let+ log = P.set_voted_for (Some req.candidate_id) t.log in
          { t with log }
        in
        let resp = Rv.make_res ~id:t.id ~term ~vote_granted:true in
        (t, [ Ac.ResetElectionTimeout; Ac.RequestVoteResponse (resp, mvar) ])
      in
      let vote () =
        let* log_at_least_as_up_to_date =
          (* check that candidate's log is at least as up to date as receiver's log *)
          let+ last_log_index, last_log_term = get_last_entry t.log in
          (* if last_entries have different terms then later term is more up-to-date *)
          if last_log_term > req.last_log_term then false
          else if last_log_term < req.last_log_term then true
          else if
            (* if have same term, the longer log is more up-to-date *)
            last_log_index > req.last_log_index
          then false
          else (* last_log_index <= req.last_log_index *) true
        in
        if not log_at_least_as_up_to_date then disallow_vote t else allow_vote t
      in
      match voted_for with
      | None -> vote ()
      | Some i when i = req.candidate_id -> vote ()
      | Some _ -> disallow_vote t

  let handle_request_votes_response (t : t) (res : Rv.res) =
    let* current_term = P.current_term t.log in
    if res.term > current_term then become_follower t res.term
    else
      let votes = t.votes_received in
      if res.term = current_term && res.vote_granted then
        let vote_count = votes + 1 in
        if majority t vote_count then become_leader t
        else
          let t = { t with votes_received = vote_count } in
          Lwt.return (t, [])
      else Lwt.return (t, [])

  let handle_command (t : t) ((command, mvar) : C.input * C.output Lwt_mvar.t) =
    match t.stage with
    | Follower | Candidate ->
        let+ voted_for = P.voted_for t.log in
        (t, [ Ac.CommandResponse (C.NotLeader voted_for, mvar) ])
    | Leader ->
        let* term = P.current_term t.log in
        let* last_log_index, _ = get_last_entry t.log in
        let new_index = last_log_index + 1 in
        let* log = P.insert new_index (P.make_entry ~term ~command) t.log in
        let replicating = CommandMap.add new_index mvar t.replicating in
        let t = { t with replicating; log } in
        handle_send_heartbeat t

  let handle_add_peer (t : t) id =
    let _peer = make_peer ~id ~voting:false in
    Lwt.return (t, [])

  let handle t event =
    match event with
    | Ev.ElectionTimeout -> handle_timeout t
    | Ev.SendHeartbeat -> handle_send_heartbeat t
    | Ev.AppendEntriesRequest req -> handle_append_entries_request t req
    | Ev.AppendEntriesResponse res -> handle_append_entries_response t res
    | Ev.RequestVoteRequest req -> handle_request_votes_request t req
    | Ev.RequestVoteResponse res -> handle_request_votes_response t res
    | Ev.CommandReceived com -> handle_command t com
    | Ev.AddPeer id -> handle_add_peer t id
end
