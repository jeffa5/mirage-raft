module type S = sig
  type ae_arg [@@deriving show]

  type ae_res [@@deriving show]

  type rv_arg [@@deriving show]

  type rv_res [@@deriving show]

  type t =
    | Timeout
    | SendHeartbeat
    | AppendEntriesRequest of (ae_arg * (ae_res Lwt_mvar.t[@opaque]))
    | AppendEntriesResponse of ae_res
    | RequestVotesRequest of (rv_arg * (rv_res Lwt_mvar.t[@opaque]))
    | RequestVotesResponse of rv_res
  [@@deriving show]
end

module Make (Ae : Append_entries.S) (Rv : Request_votes.S) = struct
  type ae_arg = Ae.args [@@deriving show]

  type ae_res = Ae.res [@@deriving show]

  type rv_arg = Rv.args [@@deriving show]

  type rv_res = Rv.res [@@deriving show]

  type t =
    | Timeout
    | SendHeartbeat
    | AppendEntriesRequest of (ae_arg * (ae_res Lwt_mvar.t[@opaque]))
    | AppendEntriesResponse of ae_res
    | RequestVotesRequest of (rv_arg * (rv_res Lwt_mvar.t[@opaque]))
    | RequestVotesResponse of rv_res
  [@@deriving show]
end
