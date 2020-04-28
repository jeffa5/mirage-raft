(** persistent log *)

module type S = sig
  type t [@@deriving sexp]

  type entry = { term : int; content : string } [@@deriving sexp]

  val empty : t

  val current_term : t -> int Lwt.t

  val voted_for : t -> int option Lwt.t

  val insert : t -> int -> entry -> t Lwt.t

  val get : t -> int -> entry option Lwt.t

  val delete_since : t -> int -> t Lwt.t
end
