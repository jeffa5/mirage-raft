module Make
    (P : Plog.S)
    (Ae : Append_entries.S)
    (Rv : Request_votes.S)
    (S : State.S with type plog := P.t)
    (Ev : Event.S with type ae_res := Ae.res and type rv_res := Rv.res) =
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
        let s =
          S.make_candidate
            ~votes_received:(proposed_term, votes + 1)
            ~server:s.server ~volatile:s.volatile ~persistent:s.persistent
        in
        (* check the updated vote count for a majority *)
        (S.Candidate s, [])
      else
        (* ignore the vote since it is for an incorrect term, maybe a previous vote *)
        (S.Candidate s, [])

  let handle (s : S.candidate) event =
    (* start election timer  (timeout -> candidate again)*)
    (* discovers current leader or new term (-> follower) *)
    (* receives votes from majority of servers ( -> leader )*)
    match event with
    | Ev.Timeout -> (S.Candidate s, [])
    | Ev.SendHeartbeat -> (S.Candidate s, [])
    | Ev.AppendEntriesRequest ae -> handle_append_entries_request s ae
    | Ev.AppendEntriesResponse res -> handle_append_entries_response s res
    | Ev.RequestVotesRequest rv -> handle_request_votes_request s rv
    | Ev.RequestVotesResponse res -> handle_request_votes_response s res
end
