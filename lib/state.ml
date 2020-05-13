open Sexplib0.Sexp_conv
open Lwt.Syntax
open Asetmap

module Make
    (M : Machine.S)
    (P : Plog.S with type command = M.input)
    (Ae : Append_entries.S with type plog_entry = P.entry)
    (Rv : Request_votes.S)
    (Ev : Event.S
            with type ae_args := Ae.args
             and type ae_res := Ae.res
             and type rv_arg := Rv.args
             and type rv_res := Rv.res
             and type command_input := M.input
             and type command_output := M.output)
    (Ac : Action.S
            with type ae_args := Ae.args
             and type ae_res := Ae.res
             and type rv_arg := Rv.args
             and type rv_res := Rv.res
             and type command_output := M.output) =
struct
  module CommandMap = struct
    module Mp = Map.Make (Int)
    include Mp

    type t = M.output option Lwt_mvar.t Mp.t

    let t_of_sexp s =
      [%of_sexp: (int * (M.output option Lwt_mvar.t[@opaque])) list] s
      |> Mp.of_list

    let sexp_of_t t =
      Mp.to_list t
      |> [%sexp_of: (int * (M.output option Lwt_mvar.t[@opaque])) list]
  end

  type stage = Leader | Candidate | Follower [@@deriving sexp]

  type peer = {
    id : int;
    address : Uri_sexp.t;
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
    machine : M.t; [@default M.v ()]
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
              let prev_log_index = max 0 (peer.next_index - 1) in
              let* prev_entry = P.get t.log prev_log_index in
              let prev_log_term =
                match prev_entry with None -> -1 | Some e -> e.term
              in
              if t.last_applied >= peer.next_index then
                let+ entries = P.get_from t.log peer.next_index in
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
    let* log = P.set_voted_for t.log None in
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
    let* log = P.set_current_term t.log term in
    (* vote for self *)
    let* log = P.set_voted_for log (Some t.id) in
    let t = { t with log; stage = Candidate; votes_received = 1 } in
    (* send request_vote rpcs to all other servers *)
    let+ request_votes =
      let+ last_log_index, last_log_term = get_last_entry t.log in
      let rv_args =
        Rv.make_args ~term ~candidate_id:t.id ~last_log_index ~last_log_term
      in
      List.map
        (fun (p : peer) -> Ac.RequestVotesRequest (p.id, rv_args))
        t.peers
    in
    (t, Ac.ResetElectionTimeout :: request_votes)

  let become_follower (t : t) term =
    let* log = P.set_current_term t.log term in
    let+ log = P.set_voted_for log None in
    ({ t with log; stage = Follower }, [ Ac.ResetElectionTimeout ])

  let apply_entries_to_machine t =
    let saved_commit_index = t.commit_index in
    let* current_term = P.current_term t.log in
    let* entries_since_commit_index =
      P.get_from t.log (saved_commit_index + 1)
    in
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

            if match_count * 2 > List.length t.peers + 1 then i else ci
          else ci)
        saved_commit_index
        (List.mapi
           (fun i e -> (i + saved_commit_index + 1, e))
           entries_since_commit_index)
    in
    if commit_index <> saved_commit_index then
      let t = { t with commit_index } in
      if t.commit_index <= t.last_applied then Lwt.return (t, [])
      else
        let t = { t with last_applied = t.commit_index } in
        let+ entries =
          List.init (t.commit_index - t.last_applied) (fun i ->
              i + t.last_applied)
          |> Lwt_list.filter_map_s (fun i ->
                 let+ entry = P.get t.log i in
                 match entry with None -> None | Some e -> Some (i, e))
        in

        List.fold_left
          (fun (t, actions) ((i, e) : int * P.entry) ->
            let command_mvar = CommandMap.find i t.replicating in
            match command_mvar with
            | None -> (t, actions)
            | Some mvar ->
                let machine, output = M.apply e.command t.machine in
                ( { t with machine },
                  Ac.CommandResponse (Some output, mvar) :: actions ))
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
            let* log = P.set_current_term t.log (current_term + 1) in
            become_leader { t with log }
        | _ -> become_candidate t )

  let handle_append_entries_request (t : t)
      ((req, mvar) : Ae.args * Ae.res Lwt_mvar.t) =
    let* t, actions =
      if req.leader_commit > t.last_applied then
        let last_applied = t.last_applied + 1 in
        let+ entry = P.get t.log t.last_applied in
        match entry with
        | None -> ({ t with last_applied }, [])
        | Some entry ->
            let machine, output = M.apply entry.command t.machine in
            let mvar = CommandMap.find last_applied t.replicating in
            let actions =
              match mvar with
              | None -> []
              | Some mvar -> [ Ac.CommandResponse (Some output, mvar) ]
            in
            ({ t with last_applied; machine }, actions)
      else Lwt.return (t, [])
    in
    let* t, actions =
      let* current_term = P.current_term t.log in
      if req.term > current_term then
        let+ t, a = become_follower t req.term in
        (t, actions @ a)
      else Lwt.return (t, actions)
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
            (fun (t, i) (entry : Ae.plog_entry) ->
              let* e = P.get t.log i in
              match e with
              | None -> Lwt.return (t, i + 1)
              | Some e ->
                  (* if an existing entry conflicts with a new one (same index but different terms), delete the existing entry and all that follow it *)
                  if e.term = entry.term then Lwt.return (t, i + 1)
                  else
                    let+ log = P.delete_from t.log i in
                    ({ t with log }, i + 1))
            (t, req.prev_log_index + 1)
            req.entries
        in
        let* t, _ =
          Lwt_list.fold_left_s
            (fun (t, i) entry ->
              (* append any new entries not already in the log *)
              let+ log = P.insert t.log i entry in
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
      let* prev_log_index_entry = P.get t.log req.prev_log_index in
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
                    let match_index = max 0 (next_index - 1) in
                    let p = { p with next_index; match_index } in
                    Lwt.return (t, p :: ps, acs)
                  else
                    (* decrement next_index and retry *)
                    let next_index = max 0 (p.next_index - 1) in
                    let p = { p with next_index } in
                    let* entries = P.get_from t.log next_index in
                    let prev_log_index = max 0 (next_index - 1) in
                    let+ prev_entry = P.get t.log prev_log_index in
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
        (t, [ Ac.ResetElectionTimeout; Ac.RequestVotesResponse (resp, mvar) ])
    else
      let* voted_for = P.voted_for t.log in
      let disallow_vote t =
        let resp = Rv.make_res ~id:t.id ~term ~vote_granted:false in
        Lwt.return
          (t, [ Ac.ResetElectionTimeout; Ac.RequestVotesResponse (resp, mvar) ])
      in
      let allow_vote t =
        let+ t =
          let+ log = P.set_voted_for t.log (Some req.candidate_id) in
          { t with log }
        in
        let resp = Rv.make_res ~id:t.id ~term ~vote_granted:true in
        (t, [ Ac.ResetElectionTimeout; Ac.RequestVotesResponse (resp, mvar) ])
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
        if 2 * vote_count > List.length t.peers + 1 then become_leader t
        else
          let t = { t with votes_received = vote_count } in
          Lwt.return (t, [])
      else Lwt.return (t, [])

  let handle_command (t : t)
      ((command, mvar) : M.input * M.output option Lwt_mvar.t) =
    match t.stage with
    | Follower | Candidate -> Lwt.return (t, [ Ac.CommandResponse (None, mvar) ])
    | Leader ->
        let commit_index = t.commit_index + 1 in
        let* term = P.current_term t.log in
        let* log = P.insert t.log commit_index (P.make_entry ~term ~command) in
        let replicating = CommandMap.add t.commit_index mvar t.replicating in
        let t = { t with replicating; log; commit_index } in
        handle_send_heartbeat t

  let handle t event =
    match event with
    | Ev.ElectionTimeout -> handle_timeout t
    | Ev.SendHeartbeat -> handle_send_heartbeat t
    | Ev.AppendEntriesRequest req -> handle_append_entries_request t req
    | Ev.AppendEntriesResponse res -> handle_append_entries_response t res
    | Ev.RequestVotesRequest req -> handle_request_votes_request t req
    | Ev.RequestVotesResponse res -> handle_request_votes_response t res
    | Ev.CommandReceived com -> handle_command t com
end
