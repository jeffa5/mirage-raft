let ( let* ) = Lwt.bind

let ( let+ ) a b = Lwt.map b a

module type PLOG = sig
  type t

  type entry = { term : int; content : string }

  val empty : t

  val insert : t -> int -> entry -> t Lwt.t

  val get : t -> int -> entry option Lwt.t

  val delete_since : t -> int -> t Lwt.t
end

module type STATE = sig
  type plog
  (** Persistent state on all servers
 *
 *  Updated on stable storage before responding to RPCs
 * *)

  type persistent = {
    current_term : int;
        (** latest term server has seen (initialised to 0 on first boot, increases monotonically) *)
    voted_for : int option;
        (** candidate id that received vote in current term (or null if none) *)
    log : plog;
        (** log entries; each entry contains command for state machine and term when entry was received by leader (first index is 1) *)
  }

  (** Volatile state on all servers *)

  type volatile = {
    commit_index : int;
        (** index of highest log entry known to be committed (initialised to 0, increases monotonically) *)
    last_applied : int;
        (** index of highest log entry applied to state machine (initialised to 0, increases monotonically) *)
  }

  (** Volatile state on leaders
 *
 *  Reinitialised after election
 * *)

  type volatile_leader = {
    next_index : int list;
        (** for each server, index of the next log entry to send to that server (initialised to leader last log index + 1) *)
    match_index : int list;
        (** for each server, index of highest log entry known to be replicated on server (initialised to 0, increases monotonically) *)
  }

  type leader_state = {
    persistent : persistent;
    volatile : volatile;
    volatile_leader : volatile_leader;
  }

  type state = { persistent : persistent; volatile : volatile }

  type t = Leader of leader_state | Candidate of state | Follower of state
end

(* if voted_for is null  or candidate_id, and candidate's log is at least as up-to-date as receiver's log, grant vote *)
module Make
    (P : Plog.S)
    (S : State.S with type plog := P.t)
    (Ae : Append_entries.S with type plog_entry := P.entry)
    (Rv : Request_votes.S) =
struct
  let handle_request_votes (s : S.state) (rv, m) =
    let rv : Rv.args = rv in
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
        ({ term = s.persistent.current_term; vote_granted } : Rv.res)
    in
    s

  let handle_append_entries (s : S.state) (ae, m) =
    let ae : Ae.args = ae in
    let* resp =
      if
        (* reply false if term < currentTerm *)
        ae.term < s.persistent.current_term
      then
        Lwt.return
          ({ term = s.persistent.current_term; success = false } : Ae.res)
      else
        let* entry = P.get s.persistent.log ae.prev_log_index in
        match entry with
        | None ->
            Lwt.return
              ({ term = s.persistent.current_term; success = false } : Ae.res)
        | Some entry ->
            if entry.term <> ae.prev_log_term then
              (* reply false if log  doesn't contain  an entry  at prev_log_index whose term matches prev_log_term*)
              Lwt.return
                ({ term = s.persistent.current_term; success = false } : Ae.res)
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
                  (ae.prev_log_index + 1, s.persistent.log)
                  ae.entries
              in
              Lwt.return
                ({ term = s.persistent.current_term; success = false } : Ae.res)
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
    | `Timeout -> Lwt.return @@ S.Candidate s
    | `SendHeartbeat -> Lwt.return @@ S.Follower s
    | `AppendEntries ae ->
        let+ s = handle_append_entries s ae in
        S.Follower s
    | `RequestVotes rv ->
        let+ s = handle_request_votes s rv in
        S.Follower s
end
