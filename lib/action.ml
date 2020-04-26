module type S = sig
  type ae_arg

  type ae_res

  type rv_arg

  type rv_res

  type t =
    | AppendEntriesRequest of ae_arg
    | AppendEntriesResponse of (ae_res * ae_res Lwt_mvar.t)
    | RequestVotesRequest of rv_arg
    | RequestVotesResponse of (rv_res * rv_res Lwt_mvar.t)
    | ResetElectionTimer

  val string : t -> string
end

module Make (Ae : Append_entries.S) (Rv : Request_votes.S) :
  S
    with type ae_arg = Ae.args
     and type ae_res = Ae.res
     and type rv_arg = Rv.args
     and type rv_res = Rv.res = struct
  type ae_arg = Ae.args

  type ae_res = Ae.res

  type rv_arg = Rv.args

  type rv_res = Rv.res

  type t =
    | AppendEntriesRequest of ae_arg
    | AppendEntriesResponse of (ae_res * ae_res Lwt_mvar.t)
    | RequestVotesRequest of rv_arg
    | RequestVotesResponse of (rv_res * rv_res Lwt_mvar.t)
    | ResetElectionTimer

  let string = function
    | AppendEntriesRequest _ -> "appendentriesrequest"
    | AppendEntriesResponse _ -> "appendentriesresponse"
    | RequestVotesRequest _ -> "requestvotesrequest"
    | RequestVotesResponse _ -> "requestvotesresponse"
    | ResetElectionTimer -> "resetelectiontimer"
end
