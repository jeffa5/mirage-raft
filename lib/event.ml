open Sexplib0.Sexp_conv

module type S = sig
  type ae_args [@@deriving sexp]

  type ae_res [@@deriving sexp]

  type rv_arg [@@deriving sexp]

  type rv_res [@@deriving sexp]

  type command_input [@@deriving sexp]

  type command_output [@@deriving sexp]

  type t =
    | ElectionTimeout
    | SendHeartbeat
    | AppendEntriesRequest of (ae_args * (ae_res Lwt_mvar.t[@opaque]))
    | AppendEntriesResponse of (ae_args * ae_res)
    | RequestVoteRequest of (rv_arg * (rv_res Lwt_mvar.t[@opaque]))
    | RequestVoteResponse of rv_res
    | CommandReceived of (command_input * (command_output Lwt_mvar.t[@opaque]))
    | AddPeer of int
  [@@deriving sexp]
end

module Make (Ae : Append_entries.S) (Rv : Request_vote.S) (C : Command.S) :
  S
    with type ae_args = Ae.args
     and type ae_res = Ae.res
     and type rv_arg = Rv.args
     and type rv_res = Rv.res
     and type command_input = C.input
     and type command_output = C.output = struct
  type ae_args = Ae.args [@@deriving sexp]

  type ae_res = Ae.res [@@deriving sexp]

  type rv_arg = Rv.args [@@deriving sexp]

  type rv_res = Rv.res [@@deriving sexp]

  type command_input = C.input [@@deriving sexp]

  type command_output = C.output [@@deriving sexp]

  type t =
    | ElectionTimeout
    | SendHeartbeat
    | AppendEntriesRequest of (ae_args * (ae_res Lwt_mvar.t[@opaque]))
    | AppendEntriesResponse of (ae_args * ae_res)
    | RequestVoteRequest of (rv_arg * (rv_res Lwt_mvar.t[@opaque]))
    | RequestVoteResponse of rv_res
    | CommandReceived of (command_input * (command_output Lwt_mvar.t[@opaque]))
    | AddPeer of int
  [@@deriving sexp]
end
