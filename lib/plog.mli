(** persistent log *)

type t

type entry = { term : int; content : string }

val empty : t

val insert : t -> int -> entry -> t Lwt.t

val get : t -> int -> entry option Lwt.t

val delete_since : t -> int -> t Lwt.t
