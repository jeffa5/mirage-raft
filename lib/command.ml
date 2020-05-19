module type S = sig
  type machine_input [@@deriving sexp]

  type input =
    | MachineInput of machine_input
    | AddPeers of int list
    | RemovePeers of int list
  [@@deriving sexp]

  type output = Result of input | NotLeader of int option [@@deriving sexp]
end
