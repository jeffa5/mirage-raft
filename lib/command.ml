module type S = sig
  type machine_input [@@deriving sexp]

  type t =
    | MachineInput of machine_input
    | AddPeers of int list
    | RemovePeers of int list
  [@@deriving sexp]
end
