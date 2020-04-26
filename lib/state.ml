module type S = sig
  type server = {
    votes_received : int; [@default 0]
    self_id : int;
    peers : (int * Uri.t) list;
  }
  [@@deriving make, show { with_path = false }]

  type plog [@@deriving show { with_path = false }]
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
  [@@deriving make, show { with_path = false }]

  (** Volatile state on all servers *)

  type volatile = {
    commit_index : int; [@default 0]
        (** index of highest log entry known to be committed (initialised to 0, increases monotonically) *)
    last_applied : int; [@default 0]
        (** index of highest log entry applied to state machine (initialised to 0, increases monotonically) *)
  }
  [@@deriving make, show { with_path = false }]

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
  [@@deriving make, show { with_path = false }]

  type leader_state = {
    server : server;
    persistent : persistent;
    volatile : volatile;
    volatile_leader : volatile_leader;
  }
  [@@deriving make, show { with_path = false }]

  type state = { server : server; persistent : persistent; volatile : volatile }
  [@@deriving make, show { with_path = false }]

  type t = Leader of leader_state | Candidate of state | Follower of state
  [@@deriving show { with_path = false }]
end

module Make (P : Plog.S) : S with type plog := P.t = struct
  type server = {
    votes_received : int; [@default 0]
    self_id : int;
    peers : (int * Uri.t) list;
  }
  [@@deriving make, show { with_path = false }]

  type plog = P.t [@@deriving show { with_path = false }]
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
  [@@deriving make, show { with_path = false }]

  (** Volatile state on all servers *)

  type volatile = {
    commit_index : int; [@default 0]
        (** index of highest log entry known to be committed (initialised to 0, increases monotonically) *)
    last_applied : int; [@default 0]
        (** index of highest log entry applied to state machine (initialised to 0, increases monotonically) *)
  }
  [@@deriving make, show { with_path = false }]

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
  [@@deriving make, show { with_path = false }]

  type leader_state = {
    server : server;
    persistent : persistent;
    volatile : volatile;
    volatile_leader : volatile_leader;
  }
  [@@deriving make, show { with_path = false }]

  type state = { server : server; persistent : persistent; volatile : volatile }
  [@@deriving make, show { with_path = false }]

  type t = Leader of leader_state | Candidate of state | Follower of state
  [@@deriving show { with_path = false }]
end
