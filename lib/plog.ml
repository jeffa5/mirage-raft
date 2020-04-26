(** persistent log *)

module type S = sig
  type t [@@deriving show]

  type entry = { term : int; content : string } [@@deriving show]

  val empty : t

  val insert : t -> int -> entry -> t Lwt.t

  val get : t -> int -> entry option Lwt.t

  val delete_since : t -> int -> t Lwt.t
end
