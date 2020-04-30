(** persistent log *)

module type S = sig
  type t [@@deriving sexp]

  type entry = { term : int; command : string } [@@deriving sexp]

  val v : unit -> t Lwt.t
  (** initialise the persistent log, loading data from previous state as available *)

  val current_term : t -> int Lwt.t
  (** [current_term t] returns the current term from the persistent log *)

  val voted_for : t -> int option Lwt.t
  (** [voted_for t] returns the voted for option from the persistent log *)

  val set_current_term : t -> int -> t Lwt.t
  (** [set_current_term t c] updates the log with [c] and persists the change before returning *)

  val set_voted_for : t -> int option -> t Lwt.t
  (** [set_voted_for t v] updates the log with [v] and persists the change before returning *)

  val insert : t -> int -> entry -> t Lwt.t

  val get : t -> int -> entry option Lwt.t

  val delete_since : t -> int -> t Lwt.t
end
