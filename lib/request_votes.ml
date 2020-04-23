module type S = sig
  type args = {
    term : int;
    candidate_id : int;
    last_log_index : int;
    last_log_term : int;
  }

  type res = { term : int; vote_granted : bool }

  val broadcast : args -> unit Lwt.t
  (** [request_votes] is invoked by candidates to gather votes
 *
 *  Arguments
 *  - [term]: candidate's term
 *  - [candidate_id]: candidate requesting vote
 *  - [last_log_index]: index of candidate's last log entry
 *  - [last_log_term]: term of candidate's last log entry
 *
 *  Results
 *  - [term]: current term, for candidate to update itself
 *  - [vote_granted]: true means candidate received vote
 * *)
end
