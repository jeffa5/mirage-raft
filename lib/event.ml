module type S = sig
  type ae_arg

  type ae_res

  type rv_arg

  type rv_res

  type t =
    | Timeout
    | SendHeartbeat
    | AppendEntriesRequest of (ae_arg * ae_res Lwt_mvar.t)
    | AppendEntriesResponse of ae_res
    | RequestVotesRequest of (rv_arg * rv_res Lwt_mvar.t)
    | RequestVotesResponse of rv_res

  val string : t -> string
end

module Make (Ae : Append_entries.S) (Rv : Request_votes.S) = struct
  type ae_arg = Ae.args

  type ae_res = Ae.res

  type rv_arg = Rv.args

  type rv_res = Rv.res

  type t =
    | Timeout
    | SendHeartbeat
    | AppendEntriesRequest of (ae_arg * ae_res Lwt_mvar.t)
    | AppendEntriesResponse of ae_res
    | RequestVotesRequest of (rv_arg * rv_res Lwt_mvar.t)
    | RequestVotesResponse of rv_res

  let string = function
    | Timeout -> "timeout"
    | SendHeartbeat -> "sendheartbeat"
    | AppendEntriesRequest (_, _) -> "appendentriesrequest"
    | AppendEntriesResponse _ -> "appendentriesresponse"
    | RequestVotesRequest (_, _) -> "requestvotesrequest"
    | RequestVotesResponse _ -> "requestvotesresponse"
end
