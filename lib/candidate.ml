let ( let* ) = Lwt.bind

let ( let+ ) a b = Lwt.map b a

module Make (P : Plog.S) (S : State.S with type plog := P.t) (Ev : Event.S) =
struct
  let handle_append_entries (s : S.state) _ae = Lwt.return @@ S.Follower s

  let handle_request_votes (s : S.state) _rv = Lwt.return @@ S.Follower s

  let handle (s : S.state) event =
    (* thread to send rv rpc to each peer
     *
     * wait for condition variable to reach majority value then return it
     *)
    (* thread for election timer listening for ae *)

    (* start election timer  (timeout -> candidate again)*)
    (* discovers current leader or new term (-> follower) *)
    (* receives votes from majority of servers ( -> leader )*)
    match event with
    | Ev.Timeout ->
        Lwt.return
        @@ S.Candidate { volatile = s.volatile; persistent = s.persistent }
    | Ev.SendHeartbeat -> Lwt.return @@ S.Candidate s
    | Ev.AppendEntriesRequest ae -> handle_append_entries s ae
    | Ev.AppendEntriesResponse _ -> assert false
    | Ev.RequestVotesRequest rv -> handle_request_votes s rv
    | Ev.RequestVotesResponse _ -> assert false
end
