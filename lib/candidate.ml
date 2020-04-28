open Lwt.Syntax

module Make
    (P : Plog.S)
    (Ae : Append_entries.S)
    (Rv : Request_votes.S)
    (S : State.S with type plog := P.t)
    (Ev : Event.S
            with type ae_res := Ae.res
             and type rv_res := Rv.res
             and type rv_arg := Rv.args)
    (Ac : Action.S
            with type ae_arg = Ae.args
             and type ae_res = Ae.res
             and type rv_arg = Rv.args
             and type rv_res = Rv.res) =
struct
  let handle_append_entries_request (s : S.candidate) _ae =
    let s = S.make_follower ~server:s.server ~log:s.log ~volatile:s.volatile in
    (S.Follower s, [])

  let handle_append_entries_response (s : S.candidate) (res : Ae.res) =
    let* current_term = P.current_term s.log in
    if res.term > current_term then
      let+ s =
        let* log = P.set_current_term s.log res.term in
        let+ log = P.set_voted_for log None in
        S.make_follower ~server:s.server ~log ~volatile:s.volatile
      in
      (S.Follower s, [])
    else (* ignore this response *) Lwt.return (S.Candidate s, [])

  let handle_request_votes_request (s : S.candidate) (rv, m) =
    let rv : Rv.args = rv in
    let* current_term = P.current_term s.log in
    if rv.term > current_term then
      let s =
        S.make_follower ~server:s.server ~log:s.log ~volatile:s.volatile
      in
      Lwt.return (S.Follower s, [])
    else if rv.term = current_term then
      let* voted_for = P.voted_for s.log in
      match voted_for with
      | None ->
          (* election reset event *)
          let+ s =
            let+ log = P.set_voted_for s.log (Some rv.candidate_id) in
            S.make_candidate ~votes_received:s.votes_received ~server:s.server
              ~log ~volatile:s.volatile
          in
          let resp = ({ term = current_term; vote_granted = true } : Rv.res) in
          ( S.Candidate s,
            [ Ac.ResetElectionTimer; Ac.RequestVotesResponse (resp, m) ] )
      | Some i when i = rv.candidate_id ->
          let resp = ({ term = current_term; vote_granted = true } : Rv.res) in
          Lwt.return
            ( S.Candidate s,
              [ Ac.ResetElectionTimer; Ac.RequestVotesResponse (resp, m) ] )
      | Some _ ->
          let resp = ({ term = current_term; vote_granted = false } : Rv.res) in
          Lwt.return (S.Candidate s, [ Ac.RequestVotesResponse (resp, m) ])
    else
      let resp = ({ term = current_term; vote_granted = false } : Rv.res) in
      Lwt.return (S.Candidate s, [ Ac.RequestVotesResponse (resp, m) ])

  let handle_request_votes_response (s : S.candidate) (res : Rv.res) =
    let* current_term = P.current_term s.log in
    if res.term > current_term then
      let+ s =
        let* log = P.set_current_term s.log res.term in
        let+ log = P.set_voted_for log None in
        S.make_follower ~server:s.server ~log ~volatile:s.volatile
      in
      (S.Follower s, [])
    else
      let proposed_term, votes = s.votes_received in
      if res.term = proposed_term && res.vote_granted then
        (* need to update the vote count *)
        let vote_count = votes + 1 in
        if 2 * vote_count > List.length s.server.peers + 1 then
          (* we have a majority so can become a leader *)
          let s =
            let volatile_leader = S.make_volatile_leader () in
            S.make_leader ~server:s.server ~volatile:s.volatile ~log:s.log
              ~volatile_leader
          in
          Lwt.return
            ( S.Leader s,
              [
                Ac.AppendEntriesRequest
                  (Ae.make_args ~term:current_term ~leader_id:s.server.self_id
                     ~prev_log_index:s.volatile.commit_index
                     ~prev_log_term:s.volatile.commit_index
                     ~leader_commit:s.volatile.commit_index ());
              ] )
        else
          let s =
            S.make_candidate
              ~votes_received:(proposed_term, vote_count)
              ~server:s.server ~volatile:s.volatile ~log:s.log
          in
          Lwt.return (S.Candidate s, [])
      else
        (* ignore the vote since it is for an incorrect term, maybe a previous vote *)
        Lwt.return (S.Candidate s, [])

  let handle (s : S.candidate) event =
    match event with
    | Ev.ElectionTimeout ->
        (* start new election *) Lwt.return @@ (S.Candidate s, [])
    | Ev.SendHeartbeat ->
        (* candidates do not send append entries requests *)
        Lwt.return @@ (S.Candidate s, [])
    | Ev.AppendEntriesRequest ae ->
        Lwt.return @@ handle_append_entries_request s ae
    | Ev.AppendEntriesResponse res -> handle_append_entries_response s res
    | Ev.RequestVotesRequest rv -> handle_request_votes_request s rv
    | Ev.RequestVotesResponse res -> handle_request_votes_response s res
end
