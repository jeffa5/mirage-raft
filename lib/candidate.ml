module Make (P : Plog.S) (S : State.S with type plog := P.t) (Ev : Event.S) =
struct
  let handle_append_entries (s : S.state) _ae = (S.Follower s, [])

  let handle_request_votes (s : S.state) _rv = (S.Follower s, [])

  let handle (s : S.state) event =
    (* start election timer  (timeout -> candidate again)*)
    (* discovers current leader or new term (-> follower) *)
    (* receives votes from majority of servers ( -> leader )*)
    match event with
    | Ev.Timeout ->
        ( S.Candidate
            {
              server = s.server;
              volatile = s.volatile;
              persistent = s.persistent;
            },
          [] )
    | Ev.SendHeartbeat -> (S.Candidate s, [])
    | Ev.AppendEntriesRequest ae -> handle_append_entries s ae
    | Ev.AppendEntriesResponse _ -> assert false
    | Ev.RequestVotesRequest rv -> handle_request_votes s rv
    | Ev.RequestVotesResponse _ -> assert false
end
