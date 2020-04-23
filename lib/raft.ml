(** main initialisation *)
let ( let* ) = Lwt.bind

let ( let+ ) a b = Lwt.map b a

module Make
    (Time : Mirage_time.S)
    (P : Plog.S)
    (Ae : Append_entries.S with type plog_entry = P.entry)
    (Rv : Request_votes.S) =
struct
  module S = State.Make (P)
  module Ev = Event.Make (Ae) (Rv)
  module Leader = Leader.Make (P) (S) (Ae) (Ev)
  module Candidate = Candidate.Make (P) (S) (Ev)
  module Follower = Follower.Make (P) (S) (Ae) (Rv) (Ev)

  type t = {
    id : int;
    peer_ids : int list;
    state : S.t;
    ae_requests : (Ae.args * Ae.res Lwt_mvar.t) Lwt_stream.t;
    ae_responses : Ae.res Lwt_stream.t;
    rv_requests : (Rv.args * Rv.res Lwt_mvar.t) Lwt_stream.t;
    rv_responses : Rv.res Lwt_stream.t;
  }

  let v ?(current_term = 0) ?(voted_for = None) ?(log = P.empty) ae_requests
      ae_responses rv_requests rv_responses id peer_ids =
    let initial_state : S.state =
      let persistent : S.persistent = { current_term; voted_for; log } in
      let volatile : S.volatile = { commit_index = 0; last_applied = 0 } in
      { persistent; volatile }
    in
    {
      id;
      peer_ids;
      state = S.Follower initial_state;
      ae_requests;
      ae_responses;
      rv_requests;
      rv_responses;
    }

  let handle (t : t) =
    let* () = Logs_lwt.info (fun f -> f "Starting raft") in
    (* events is the stream where all events come through, i.e.
     * Timeouts
     * Append Entries requests
     * Request Votes requests *)
    let events, push_event = Lwt_stream.create () in

    (* receive the call to append entries or a timeout and push the event to events *)
    let append_entries_requests () =
      let rec loop () =
        let election_timeout =
          let+ () = Time.sleep_ns (Duration.of_sec 1) in
          Some Ev.Timeout
        in
        let get_ae_request =
          let+ ae = Lwt_stream.get t.ae_requests in
          match ae with
          | None -> None
          | Some ae -> Some (Ev.AppendEntriesRequest ae)
        in
        let* event = Lwt.pick [ election_timeout; get_ae_request ] in
        match event with
        | None -> Lwt.return_unit
        | Some event ->
            push_event (Some event);
            loop ()
      in
      loop ()
    in
    Lwt.async append_entries_requests;

    let append_entries_responses () =
      let rec loop () =
        let* ae = Lwt_stream.get t.ae_responses in
        match ae with
        | None -> Lwt.return_unit
        | Some ae ->
            push_event (Some (Ev.AppendEntriesResponse ae));
            loop ()
      in
      loop ()
    in
    Lwt.async append_entries_responses;

    (* receives the call to request votes and pushes the event to events *)
    let request_votes_requests () =
      let rec loop () =
        let* rv = Lwt_stream.get t.rv_requests in
        match rv with
        | None -> Lwt.return_unit
        | Some rv ->
            push_event (Some (Ev.RequestVotesRequest rv));
            loop ()
      in
      loop ()
    in
    Lwt.async request_votes_requests;

    let request_votes_responses () =
      let rec loop () =
        let* rv = Lwt_stream.get t.rv_responses in
        match rv with
        | None -> Lwt.return_unit
        | Some rv ->
            push_event (Some (Ev.RequestVotesResponse rv));
            loop ()
      in
      loop ()
    in
    Lwt.async request_votes_responses;

    (* ticker for leader to send heartbeats, can be ignored by others *)
    let heartbeat_ticker () =
      let rec loop () =
        let* () = Time.sleep_ns (Duration.of_sec 1) in
        push_event (Some Ev.SendHeartbeat);
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
          let* () =
            Logs_lwt.info (fun f -> f "Received event %s" (Ev.string event))
          in
          let* s =
            match s with
            | S.Follower s -> Follower.handle s event
            | S.Candidate s -> Candidate.handle s event
            | S.Leader ls -> Leader.handle ls event
          in
          loop s
    in
    loop t.state
end
