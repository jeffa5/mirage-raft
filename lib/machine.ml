(* the state machine of the client which we are replicating *)

module type S = sig
  type input [@@deriving sexp]
end
