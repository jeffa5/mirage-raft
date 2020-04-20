let ( let* ) = Lwt.bind

let ( let+ ) a b = Lwt.map b a

let handle_append_entries (s : State.state) _ae = Lwt.return @@ State.Follower s

let handle_request_votes (s : State.state) _rv = Lwt.return @@ State.Follower s

let handle (s : State.state) event =
  (* thread to send rv rpc to each peer
   *
   * wait for condition variable to reach majority value then return it
   *)
  (* thread for election timer listening for ae *)

  (* start election timer  (timeout -> candidate again)*)
  (* discovers current leader or new term (-> follower) *)
  (* receives votes from majority of servers ( -> leader )*)
  match event with
  | `Timeout ->
      Lwt.return
      @@ State.Candidate { volatile = s.volatile; persistent = s.persistent }
  | `SendHeartbeat -> Lwt.return @@ State.Candidate s
  | `AppendEntries ae -> handle_append_entries s ae
  | `RequestVotes rv -> handle_request_votes s rv
