open Sexplib0.Sexp_conv

module type S = sig
  type ae_args [@@deriving sexp]

  type ae_res [@@deriving sexp]

  type rv_arg [@@deriving sexp]

  type rv_res [@@deriving sexp]

  type command [@@deriving sexp]

  type t =
    | AppendEntriesRequest of int * ae_args
    | AppendEntriesResponse of (ae_res * (ae_res Lwt_mvar.t[@opaque]))
    | RequestVoteRequest of int * rv_arg
    | RequestVoteResponse of (rv_res * (rv_res Lwt_mvar.t[@opaque]))
    | ResetElectionTimeout
    | CommandResponse of (command option * (command option Lwt_mvar.t[@opaque]))
  [@@deriving sexp]
end

module Make (Ae : Append_entries.S) (Rv : Request_vote.S) (C : Command.S) :
  S
    with type ae_args = Ae.args
     and type ae_res = Ae.res
     and type rv_arg = Rv.args
     and type rv_res = Rv.res
     and type command = C.t = struct
  type ae_args = Ae.args [@@deriving sexp]

  type ae_res = Ae.res [@@deriving sexp]

  type rv_arg = Rv.args [@@deriving sexp]

  type rv_res = Rv.res [@@deriving sexp]

  type command = C.t [@@deriving sexp]

  type t =
    | AppendEntriesRequest of int * ae_args
    | AppendEntriesResponse of (ae_res * (ae_res Lwt_mvar.t[@opaque]))
    | RequestVoteRequest of int * rv_arg
    | RequestVoteResponse of (rv_res * (rv_res Lwt_mvar.t[@opaque]))
    | ResetElectionTimeout
    | CommandResponse of (command option * (command option Lwt_mvar.t[@opaque]))
  [@@deriving sexp]
end
