let ( let* ) = Lwt.bind

let ( let+ ) a b = Lwt.map b a

module type AE = sig
  type args = {
    term : int;
    leader_id : int;
    prev_log_index : int;
    prev_log_term : int;
    entries : Plog.entry list;
    leader_commit : int;
  }

  type res = { term : int; success : bool }

  val f : args -> res Lwt.t
end

module Make (Ae : AE) = struct
  let handle_send_heartbeat (s : State.leader_state) =
    let ae_args : Ae.args =
      {
        term = s.persistent.current_term;
        leader_id = 0;
        prev_log_index = s.volatile.last_applied;
        prev_log_term = s.persistent.current_term;
        entries = [];
        leader_commit = s.volatile.commit_index;
      }
    in
    let* hb = Ae.f ae_args in
    if hb.term > s.persistent.current_term then
      let s : State.state =
        { volatile = s.volatile; persistent = s.persistent }
      in
      Lwt.return @@ State.Follower s
    else Lwt.return @@ State.Leader s

  let handle_append_entries (s : State.leader_state) _ae =
    Lwt.return @@ State.Leader s

  let handle_request_votes (s : State.leader_state) _rv =
    Lwt.return @@ State.Leader s

  let handle (s : State.leader_state) event =
    (* upon election: send initial empty append_entries rpcs (heartbeat) to each server; repeat during idle periods to prevent election timeouts *)
    (* if command received from client: append entry to local log, respond after entry applied to state machine *)
    (* if last_log_index >= next_index for a follower: send append_entries rpc with log entries starting at next_index *)
    (* if successful: update next_index and match_index for follower *)
    (* if append_entries fails because of log inconsistency: decrement next_index and retry *)
    (* if there exists an N such that N > commit_index, a majority of match_index[i] >= N, and log[N].term == current_term: set commit_index = N *)
    match event with
    | `Timeout ->
        Lwt.return
        @@ State.Candidate { volatile = s.volatile; persistent = s.persistent }
    | `SendHeartbeat -> handle_send_heartbeat s
    | `AppendEntries ae -> handle_append_entries s ae
    | `RequestVotes rv -> handle_request_votes s rv
end
