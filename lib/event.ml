module type S = sig
  type ae_arg [@@deriving sexp]

  type ae_res [@@deriving sexp]

  type rv_arg [@@deriving sexp]

  type rv_res [@@deriving sexp]

  type command_input [@@deriving sexp]

  type command_output [@@deriving sexp]

  type t =
    | ElectionTimeout
    | SendHeartbeat
    | AppendEntriesRequest of (ae_arg * (ae_res Lwt_mvar.t[@opaque]))
    | AppendEntriesResponse of ae_res
    | RequestVotesRequest of (rv_arg * (rv_res Lwt_mvar.t[@opaque]))
    | RequestVotesResponse of rv_res
    | CommandReceived of
        (command_input * (command_output option Lwt_mvar.t[@opaque]))
  [@@deriving sexp]
end

module Make (Ae : Append_entries.S) (Rv : Request_votes.S) (M : Machine.S) :
  S
    with type ae_arg = Ae.args
     and type ae_res = Ae.res
     and type rv_arg = Rv.args
     and type rv_res = Rv.res
     and type command_input = M.input
     and type command_output = M.output = struct
  type ae_arg = Ae.args [@@deriving sexp]

  type ae_res = Ae.res [@@deriving sexp]

  type rv_arg = Rv.args [@@deriving sexp]

  type rv_res = Rv.res [@@deriving sexp]

  type command_input = M.input [@@deriving sexp]

  type command_output = M.output [@@deriving sexp]

  type t =
    | ElectionTimeout
    | SendHeartbeat
    | AppendEntriesRequest of (ae_arg * (ae_res Lwt_mvar.t[@opaque]))
    | AppendEntriesResponse of ae_res
    | RequestVotesRequest of (rv_arg * (rv_res Lwt_mvar.t[@opaque]))
    | RequestVotesResponse of rv_res
    | CommandReceived of
        (command_input * (command_output option Lwt_mvar.t[@opaque]))
  [@@deriving sexp]
end
