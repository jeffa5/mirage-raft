module Make
    (P : Plog.S)
    (S : State.S with type plog := P.t)
    (Ae : Append_entries.S with type plog_entry := P.entry)
    (Ev : Event.S with type ae_arg = Ae.args and type ae_res = Ae.res)
    (Ac : Action.S with type ae_arg = Ae.args and type ae_res = Ae.res) =
struct
  let handle_send_heartbeat (s : S.leader) =
    let ae_args : Ae.args =
      {
        term = s.persistent.current_term;
        leader_id = s.server.self_id;
        prev_log_index = s.volatile.last_applied;
        prev_log_term = s.persistent.current_term;
        entries = [];
        leader_commit = s.volatile.commit_index;
      }
    in
    (S.Leader s, [ Ac.AppendEntriesRequest ae_args ])

  let handle_append_entries_request (s : S.leader) _ae = (S.Leader s, [])

  let handle_append_entries_response (s : S.leader) _r = (S.Leader s, [])

  let handle_request_votes_request (s : S.leader) _rv = (S.Leader s, [])

  let handle_request_votes_response (s : S.leader) _r = (S.Leader s, [])

  let handle (s : S.leader) event =
    (* if command received from client: append entry to local log, respond after entry applied to state machine *)
    (* if last_log_index >= next_index for a follower: send append_entries rpc with log entries starting at next_index *)
    (* if successful: update next_index and match_index for follower *)
    (* if append_entries fails because of log inconsistency: decrement next_index and retry *)
    (* if there exists an N such that N > commit_index, a majority of match_index[i] >= N, and log[N].term == current_term: set commit_index = N *)
    match event with
    | Ev.Timeout ->
        (* timeouts are for elections only but we are already a leader *)
        (S.Leader s, [])
    | Ev.SendHeartbeat -> handle_send_heartbeat s
    | Ev.AppendEntriesRequest ae -> handle_append_entries_request s ae
    | Ev.AppendEntriesResponse r -> handle_append_entries_response s r
    | Ev.RequestVotesRequest rv -> handle_request_votes_request s rv
    | Ev.RequestVotesResponse r -> handle_request_votes_response s r
end
