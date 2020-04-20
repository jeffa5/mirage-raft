(** main initialisation *)
let ( let* ) = Lwt.bind

let ( let+ ) a b = Lwt.map b a

type t = {
  id : int;
  peer_ids : int list;
  state : State.t;
  ae_stream :
    (Append_entries.args * Append_entries.res Lwt_mvar.t) Lwt_stream.t;
  rv_stream : (Request_votes.args * Request_votes.res Lwt_mvar.t) Lwt_stream.t;
}

let v ?(current_term = 0) ?(voted_for = None) ?(log = Plog.empty) id peer_ids =
  let ae_stream, push_ae = Lwt_stream.create () in
  let rv_stream, push_rv = Lwt_stream.create () in
  let initial_state : State.state =
    let persistent : State.persistent = { current_term; voted_for; log } in
    let volatile : State.volatile = { commit_index = 0; last_applied = 0 } in
    { persistent; volatile }
  in
  ( { id; peer_ids; state = State.Follower initial_state; ae_stream; rv_stream },
    push_ae,
    push_rv )

module type AE = sig
  type args = {
    term : int;
    leader_id : int;
    prev_log_index : int;
    prev_log_term : int;
    entries : Plog.entry list;
    leader_commit : int;
  }

  type res = { term : int; success : bool }

  val f : args -> res Lwt.t
end

(* main event loop which listens for rpc's and performs relevant actions to update the state machine *)
module Make (Time : Mirage_time.S) (Ae : AE) = struct
  module Leader = Leader.Make (Ae)

  let handle (t : t) =
    (* events is the stream where all events come through, i.e.
     * Timeouts
     * Append Entries requests
     * Request Votes requests *)
    let events, push_event = Lwt_stream.create () in

    (* receive the call to append entries or a timeout and push the event to events *)
    let append_entries () =
      let rec loop () =
        let election_timeout =
          let+ () = Time.sleep_ns (Duration.of_sec 1) in
          `Timeout
        in
        let get_ae =
          let+ ae = Lwt_stream.get t.ae_stream in
          match ae with None -> assert false | Some ae -> `AppendEntries ae
        in
        let* event = Lwt.pick [ election_timeout; get_ae ] in
        push_event (Some event);
        loop ()
      in
      loop ()
    in
    Lwt.async append_entries;

    (* receives the call to request votes and pushes the event to events *)
    let request_votes () =
      let rec loop () =
        let* rv = Lwt_stream.get t.rv_stream in
        match rv with
        | None -> Lwt.return_unit
        | Some rv ->
            push_event (Some (`RequestVotes rv));
            loop ()
      in
      loop ()
    in
    Lwt.async request_votes;

    (* ticker for leader to send heartbeats, can be ignored by others *)
    let heartbeat_ticker () =
      let rec loop () =
        let* () = Time.sleep_ns (Duration.of_sec 1) in
        push_event (Some `SendHeartbeat);
        loop ()
      in
      loop ()
    in
    Lwt.async heartbeat_ticker;

    let rec loop s =
      let* event = Lwt_stream.get events in
      match event with
      | None -> Lwt.return_unit
      | Some event ->
          let* s =
            match s with
            | State.Follower s -> Follower.handle s event
            | State.Candidate s -> Candidate.handle s event
            | State.Leader ls -> Leader.handle ls event
          in
          loop s
    in
    loop t.state
end
