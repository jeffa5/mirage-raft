module type S = sig
  type ae_arg [@@deriving sexp]

  type ae_res [@@deriving sexp]

  type rv_arg [@@deriving sexp]

  type rv_res [@@deriving sexp]

  type t =
    | Timeout
    | SendHeartbeat
    | AppendEntriesRequest of (ae_arg * (ae_res Lwt_mvar.t[@opaque]))
    | AppendEntriesResponse of ae_res
    | RequestVotesRequest of (rv_arg * (rv_res Lwt_mvar.t[@opaque]))
    | RequestVotesResponse of rv_res
  [@@deriving sexp]
end

module Make (Ae : Append_entries.S) (Rv : Request_votes.S) = struct
  type ae_arg = Ae.args [@@deriving sexp]

  type ae_res = Ae.res [@@deriving sexp]

  type rv_arg = Rv.args [@@deriving sexp]

  type rv_res = Rv.res [@@deriving sexp]

  type t =
    | Timeout
    | SendHeartbeat
    | AppendEntriesRequest of (ae_arg * (ae_res Lwt_mvar.t[@opaque]))
    | AppendEntriesResponse of ae_res
    | RequestVotesRequest of (rv_arg * (rv_res Lwt_mvar.t[@opaque]))
    | RequestVotesResponse of rv_res
  [@@deriving sexp]
end
