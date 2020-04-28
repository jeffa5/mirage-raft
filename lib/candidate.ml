module Make
    (P : Plog.S)
    (Ae : Append_entries.S)
    (Rv : Request_votes.S)
    (S : State.S with type plog := P.t)
    (Ev : Event.S with type ae_res := Ae.res and type rv_res := Rv.res)
    (Ac : Action.S
            with type ae_arg = Ae.args
             and type ae_res = Ae.res
             and type rv_arg = Rv.args
             and type rv_res = Rv.res) =
struct
  let handle_append_entries_request (s : S.candidate) _ae =
    let s =
      S.make_follower ~server:s.server ~persistent:s.persistent
        ~volatile:s.volatile
    in
    (S.Follower s, [])

  let handle_append_entries_response (s : S.candidate) (res : Ae.res) =
    if res.term > s.persistent.current_term then
      let s =
        let persistent =
          S.make_persistent ~current_term:res.term ~log:s.persistent.log ()
        in
        S.make_follower ~server:s.server ~persistent ~volatile:s.volatile
      in
      (S.Follower s, [])
    else (* ignore this response *) (S.Candidate s, [])

  let handle_request_votes_request (s : S.candidate) _rv =
    let s =
      S.make_follower ~server:s.server ~persistent:s.persistent
        ~volatile:s.volatile
    in
    (S.Follower s, [])

  let handle_request_votes_response (s : S.candidate) (res : Rv.res) =
    if res.term > s.persistent.current_term then
      let s =
        let persistent =
          S.make_persistent ~current_term:res.term ~log:s.persistent.log ()
        in
        S.make_follower ~server:s.server ~persistent ~volatile:s.volatile
      in
      (S.Follower s, [])
    else
      let proposed_term, votes = s.votes_received in
      if res.term = proposed_term && res.vote_granted then
        (* need to update the vote count *)
        let vote_count = votes + 1 in
        if vote_count > List.length s.server.peers / 2 then
          (* we have a majority so can become a leader *)
          let s =
            let volatile_leader = S.make_volatile_leader () in
            S.make_leader ~server:s.server ~volatile:s.volatile
              ~persistent:s.persistent ~volatile_leader
          in
          ( S.Leader s,
            [
              Ac.AppendEntriesRequest
                (Ae.make_args ~term:s.persistent.current_term
                   ~leader_id:s.server.self_id
                   ~prev_log_index:s.volatile.commit_index
                   ~prev_log_term:s.volatile.commit_index
                   ~leader_commit:s.volatile.commit_index ());
            ] )
        else
          let s =
            S.make_candidate
              ~votes_received:(proposed_term, vote_count)
              ~server:s.server ~volatile:s.volatile ~persistent:s.persistent
          in
          (* check the updated vote count for a majority *)
          (S.Candidate s, [])
      else
        (* ignore the vote since it is for an incorrect term, maybe a previous vote *)
        (S.Candidate s, [])

  let handle (s : S.candidate) event =
    match event with
    | Ev.ElectionTimeout -> (* start new election *) (S.Candidate s, [])
    | Ev.SendHeartbeat ->
        (* candidates do not send append entries requests *) (S.Candidate s, [])
    | Ev.AppendEntriesRequest ae -> handle_append_entries_request s ae
    | Ev.AppendEntriesResponse res -> handle_append_entries_response s res
    | Ev.RequestVotesRequest rv -> handle_request_votes_request s rv
    | Ev.RequestVotesResponse res -> handle_request_votes_response s res
end
