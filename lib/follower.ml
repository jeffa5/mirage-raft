open Lwt.Syntax

module Make
    (P : Plog.S)
    (S : State.S with type plog := P.t)
    (Ae : Append_entries.S with type plog_entry := P.entry)
    (Rv : Request_votes.S)
    (Ev : Event.S
            with type ae_arg = Ae.args
             and type ae_res = Ae.res
             and type rv_arg = Rv.args
             and type rv_res = Rv.res)
    (Ac : Action.S
            with type ae_arg = Ae.args
             and type ae_res = Ae.res
             and type rv_arg = Rv.args
             and type rv_res = Rv.res) =
struct
  let handle_timeout (s : S.follower) =
    match s.server.peers with
    | [] ->
        (* straight to leader *)
        let* current_term = P.current_term s.log in
        let* log = P.set_current_term s.log (current_term + 1) in
        let+ log = P.set_voted_for log (Some s.server.self_id) in
        let s =
          S.make_leader ~server:s.server ~log ~volatile:s.volatile
            ~volatile_leader:(S.make_volatile_leader ())
        in
        ( S.Leader s,
          [
            Ac.AppendEntriesRequest
              (Ae.make_args ~term:current_term ~leader_id:s.server.self_id
                 ~prev_log_index:s.volatile.commit_index
                 ~prev_log_term:s.volatile.commit_index
                 ~leader_commit:s.volatile.commit_index ());
          ] )
    | _ ->
        let* current_term = P.current_term s.log in
        let new_term = current_term + 1 in
        let* log = P.set_current_term s.log new_term in
        let+ log = P.set_voted_for log (Some s.server.self_id) in
        let s =
          S.make_candidate ~votes_received:(new_term, 1) ~server:s.server ~log
            ~volatile:s.volatile
        in
        let rv_args : Rv.args =
          {
            term = new_term;
            candidate_id = s.server.self_id;
            last_log_index = 0;
            last_log_term = 0;
          }
        in
        ( S.Candidate s,
          [ Ac.ResetElectionTimer; Ac.RequestVotesRequest rv_args ] )

  let handle_request_votes (s : S.follower) (rv, m) =
    let rv : Rv.args = rv in
    let* current_term = P.current_term s.log in
    let+ vote_granted =
      (* reply false if term < current_term *)
      if rv.term < current_term then Lwt.return_false
      else
        let+ voted_for = P.voted_for s.log in
        match voted_for with
        | None ->
            if (* log at least as up to date as other *) false then false
            else true
        | Some id when id = rv.candidate_id ->
            if (* log at least as up to date as other *) false then false
            else true
        | Some _id -> false
    in
    let resp = ({ term = current_term; vote_granted } : Rv.res) in
    (S.Follower s, [ Ac.RequestVotesResponse (resp, m) ])

  let handle_append_entries (s : S.follower) (ae, m) =
    let ae : Ae.args = ae in
    let* resp =
      let* current_term = P.current_term s.log in
      if (* reply false if term < currentTerm *)
         ae.term < current_term then
        Lwt.return ({ term = current_term; success = false } : Ae.res)
      else
        let* entry = P.get s.log ae.prev_log_index in
        match entry with
        | None -> Lwt.return ({ term = current_term; success = false } : Ae.res)
        | Some entry ->
            if entry.term <> ae.prev_log_term then
              (* reply false if log  doesn't contain  an entry  at prev_log_index whose term matches prev_log_term*)
              Lwt.return ({ term = current_term; success = false } : Ae.res)
            else
              let* _, _l =
                Lwt_list.fold_left_s
                  (fun (i, l) entry ->
                    let entry : P.entry = entry in
                    let* existing = P.get l i in
                    (* if an existing entry conflicts with a new one (same index but different terms), delete the existing entry and all that follow it*)
                    let* l =
                      match existing with
                      | None -> Lwt.return l
                      | Some existing ->
                          if existing.term <> entry.term then P.delete_since l i
                          else Lwt.return l
                    in

                    (* append any new entries not already in the log *)
                    let* l = P.insert l i entry in
                    Lwt.return (i + 1, l))
                  (ae.prev_log_index + 1, s.log)
                  ae.entries
              in
              Lwt.return ({ term = current_term; success = false } : Ae.res)
    in
    (* if leader_commit > commit_index, set commit_index=min(leader_commit, index of last new entry) *)
    let volatile =
      if ae.leader_commit > s.volatile.commit_index then
        {
          s.volatile with
          commit_index = min ae.leader_commit s.volatile.last_applied;
        }
      else s.volatile
    in
    let s = { s with volatile } in
    Lwt.return (S.Follower s, [ Ac.AppendEntriesResponse (resp, m) ])

  let handle_append_entries_response (s : S.follower) (res : Ae.res) =
    let* current_term = P.current_term s.log in
    if res.term > current_term then
      let+ s =
        let* log = P.set_current_term s.log res.term in
        let+ log = P.set_voted_for log None in
        S.make_follower ~server:s.server ~log ~volatile:s.volatile
      in
      (S.Follower s, [])
    else (* ignore this response *) Lwt.return (S.Follower s, [])

  let handle_request_votes_response (s : S.follower) (res : Rv.res) =
    let* current_term = P.current_term s.log in
    if res.term > current_term then
      let+ s =
        let* log = P.set_current_term s.log res.term in
        let+ log = P.set_voted_for log None in
        S.make_follower ~server:s.server ~log ~volatile:s.volatile
      in
      (S.Follower s, [])
    else
      (* ignore the response since it has nothing to do with us now *)
      Lwt.return (S.Follower s, [])

  let handle s event =
    match event with
    | Ev.ElectionTimeout -> handle_timeout s
    | Ev.SendHeartbeat -> Lwt.return @@ (S.Follower s, [])
    | Ev.AppendEntriesRequest ae -> handle_append_entries s ae
    | Ev.AppendEntriesResponse res -> handle_append_entries_response s res
    | Ev.RequestVotesRequest rv -> handle_request_votes s rv
    | Ev.RequestVotesResponse res -> handle_request_votes_response s res
end
