(** persistent log *)

module type S = sig
  type t [@@deriving sexp]

  type command

  type entry = { term : int; command : command } [@@deriving make, sexp]

  val v : unit -> t Lwt.t
  (** initialise the persistent log, loading data from previous state as available *)

  val current_term : t -> int Lwt.t
  (** [current_term t] returns the current term from the persistent log *)

  val voted_for : t -> int option Lwt.t
  (** [voted_for t] returns the voted for option from the persistent log *)

  val set_current_term : int -> t -> t Lwt.t
  (** [set_current_term c t] updates the log with [c] and persists the change before returning *)

  val set_voted_for : int option -> t -> t Lwt.t
  (** [set_voted_for v t] updates the log with [v] and persists the change before returning *)

  val insert : int -> entry -> t -> t Lwt.t
  (** [insert i e t] inserts entry [e] into [t] at index [i] *)

  val get : int -> t -> entry option Lwt.t
  (** [get i t] gets the (maybe missing) entry at index [i] *)

  val delete_from : int -> t -> t Lwt.t
  (** [delete_from i t] deletes all entries starting from index [i] inclusive *)

  val get_from : int -> t -> entry list Lwt.t
  (** [get_from i t] returns the list of entries from index [i] inclusive *)

  val last_entry : t -> (int * int) option Lwt.t
  (** [last_entry t] returns the index and entry for the last log entry, if an entry exists *)
end
