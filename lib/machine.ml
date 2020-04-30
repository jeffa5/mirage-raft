(* the state machine of the client which we are replicating *)

module type S = sig
  type t [@@deriving sexp]

  type input [@@deriving sexp]

  type output [@@deriving sexp]

  val v : unit -> t

  val apply : t -> input -> t * output
end
