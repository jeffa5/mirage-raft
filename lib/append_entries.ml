module type S = sig
  type plog_entry

  type args = {
    term : int;
    leader_id : int;
    prev_log_index : int;
    prev_log_term : int;
    entries : plog_entry list;
    leader_commit : int;
  }

  type res = { term : int; success : bool }

  val send : args -> res Lwt.t
  (** [append_entries] is invoked by leader to replicate log entries; also used as heartbeat.
 *
 *  Arguments
 *  - [term]: leader's term
 *  - [leader_id]: so follower can redirect clients
 *  - [prev_log_index]: index of log entry immediately preceding new ones
 *  - [prev_log_term]: term of [prev_log_index] entry
 *  - [entries]: log entries to store (empty for heartbeat; may send more than one for efficiency)
 *  - [leader_commit]: leader's [commit_index]
 *
 *  Results
 *  - [term]: current term, for leader to update itself
 *  - [success]: true if follower contained entry matching [prev_log_index] and [prev_log_term]
 * *)

  val recv : unit -> (args * res Lwt_mvar.t) Lwt.t
  (** [recv ()] returns an Lwt thread which waits to receive communication from peers *)
end
