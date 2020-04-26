module type S = sig
  type ae_arg [@@deriving show { with_path = false }]

  type ae_res [@@deriving show { with_path = false }]

  type rv_arg [@@deriving show { with_path = false }]

  type rv_res [@@deriving show { with_path = false }]

  type t =
    | AppendEntriesRequest of ae_arg
    | AppendEntriesResponse of (ae_res * (ae_res Lwt_mvar.t[@opaque]))
    | RequestVotesRequest of rv_arg
    | RequestVotesResponse of (rv_res * (rv_res Lwt_mvar.t[@opaque]))
    | ResetElectionTimer
  [@@deriving show { with_path = false }]
end

module Make (Ae : Append_entries.S) (Rv : Request_votes.S) :
  S
    with type ae_arg = Ae.args
     and type ae_res = Ae.res
     and type rv_arg = Rv.args
     and type rv_res = Rv.res = struct
  type ae_arg = Ae.args [@@deriving show { with_path = false }]

  type ae_res = Ae.res [@@deriving show { with_path = false }]

  type rv_arg = Rv.args [@@deriving show { with_path = false }]

  type rv_res = Rv.res [@@deriving show { with_path = false }]

  type t =
    | AppendEntriesRequest of ae_arg
    | AppendEntriesResponse of (ae_res * (ae_res Lwt_mvar.t[@opaque]))
    | RequestVotesRequest of rv_arg
    | RequestVotesResponse of (rv_res * (rv_res Lwt_mvar.t[@opaque]))
    | ResetElectionTimer
  [@@deriving show { with_path = false }]
end
