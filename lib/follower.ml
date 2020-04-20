let ( let* ) = Lwt.bind

let ( let+ ) a b = Lwt.map b a

let handle_request_votes (s : State.state) (rv, m) =
  let rv : Request_votes.args = rv in
  let vote_granted =
    (* reply false if term < current_term *)
    if rv.term < s.persistent.current_term then false
    else
      match s.persistent.voted_for with
      | None ->
          if (* log at least as up to date as other *) false then false
          else true
      | Some id when id = rv.candidate_id ->
          if (* log at least as up to date as other *) false then false
          else true
      | Some _id -> false
  in
  let+ () =
    Lwt_mvar.put m
      ({ term = s.persistent.current_term; vote_granted } : Request_votes.res)
  in
  s

(* if voted_for is null  or candidate_id, and candidate's log is at least as up-to-date as receiver's log, grant vote *)

let handle_append_entries (s : State.state) (ae, m) =
  let ae : Append_entries.args = ae in
  let* resp =
    if
      (* reply false if term < currentTerm *)
      ae.term < s.persistent.current_term
    then
      Lwt.return
        ( { term = s.persistent.current_term; success = false }
          : Append_entries.res )
    else
      let* entry = Plog.get s.persistent.log ae.prev_log_index in
      match entry with
      | None ->
          Lwt.return
            ( { term = s.persistent.current_term; success = false }
              : Append_entries.res )
      | Some entry ->
          if entry.term <> ae.prev_log_term then
            (* reply false if log  doesn't contain  an entry  at prev_log_index whose term matches prev_log_term*)
            Lwt.return
              ( { term = s.persistent.current_term; success = false }
                : Append_entries.res )
          else
            let* _, _l =
              Lwt_list.fold_left_s
                (fun (i, l) entry ->
                  let* existing = Plog.get l i in
                  (* if an existing entry conflicts with a new one (same index but different terms), delete the existing entry and all that follow it*)
                  let* l =
                    match existing with
                    | None -> Lwt.return l
                    | Some existing ->
                        if existing.term <> entry.Plog.term then
                          Plog.delete_since l i
                        else Lwt.return l
                  in

                  (* append any new entries not already in the log *)
                  let* l = Plog.insert l i entry in
                  Lwt.return (i + 1, l))
                (ae.prev_log_index + 1, s.persistent.log)
                ae.entries
            in
            Lwt.return
              ( { term = s.persistent.current_term; success = false }
                : Append_entries.res )
  in
  let* () = Lwt_mvar.put m resp in
  (* if leader_commit > commit_index, set commit_index=min(leader_commit, index of last new entry) *)
  let volatile =
    if ae.leader_commit > s.volatile.commit_index then
      {
        s.volatile with
        commit_index = min ae.leader_commit s.volatile.last_applied;
      }
    else s.volatile
  in
  Lwt.return { s with volatile }

(* start the election timer *)
(* start a thread waiting on the stream *)
(* pick between these two threads and whichever completes first dictates what we do next *)

(* handle rpcs from other servers *)
(* start election timer on append entries listener *)

let handle s event =
  match event with
  | `Timeout -> Lwt.return @@ State.Candidate s
  | `SendHeartbeat -> Lwt.return @@ State.Follower s
  | `AppendEntries ae ->
      let+ s = handle_append_entries s ae in
      State.Follower s
  | `RequestVotes rv ->
      let+ s = handle_request_votes s rv in
      State.Follower s
