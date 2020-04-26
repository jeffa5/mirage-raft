module type S = sig
  type server = { votes_received : int; self_id : int; peers : int list }

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
    server : server;
    persistent : persistent;
    volatile : volatile;
    volatile_leader : volatile_leader;
  }

  type state = { server : server; persistent : persistent; volatile : volatile }

  type t = Leader of leader_state | Candidate of state | Follower of state

  val string : t -> string
end

module Make (P : Plog.S) : S with type plog := P.t = struct
  type server = { votes_received : int; self_id : int; peers : int list }

  type plog = P.t
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
    server : server;
    persistent : persistent;
    volatile : volatile;
    volatile_leader : volatile_leader;
  }

  type state = { server : server; persistent : persistent; volatile : volatile }

  type t = Leader of leader_state | Candidate of state | Follower of state

  let string = function
    | Leader _ -> "leader"
    | Candidate _ -> "candidate"
    | Follower _ -> "follower"
end
