open Lwt.Syntax

module Make
    (Time : Mirage_time.S)
    (P : Plog.S)
    (Ae : Append_entries.S with type plog_entry = P.entry)
    (Rv : Request_votes.S) =
struct
  module S = State.Make (P)
  module Ev = Event.Make (Ae) (Rv)
  module Ac = Action.Make (Ae) (Rv)
  module Leader = Leader.Make (P) (S) (Ae) (Ev) (Ac)
  module Candidate = Candidate.Make (P) (S) (Ev)
  module Follower = Follower.Make (P) (S) (Ae) (Rv) (Ev) (Ac)

  type t = {
    state : S.t;
    ae_requests : (Ae.args * Ae.res Lwt_mvar.t) Lwt_stream.t;
    ae_responses : Ae.res Lwt_stream.t;
    rv_requests : (Rv.args * Rv.res Lwt_mvar.t) Lwt_stream.t;
    rv_responses : Rv.res Lwt_stream.t;
  }

  let v ?(current_term = 0) ?(voted_for = None) ?(log = P.empty) ae_requests
      ae_responses rv_requests rv_responses id peer_ids =
    let initial_state =
      let server = S.make_server ~self_id:id ~peers:peer_ids () in
      let persistent = S.make_persistent ~current_term ?voted_for ~log () in
      let volatile = S.make_volatile () in
      S.make_state ~server ~persistent ~volatile
    in
    {
      state = S.Follower initial_state;
      ae_requests;
      ae_responses;
      rv_requests;
      rv_responses;
    }

  let reset_election_timeout = Lwt_mvar.create_empty ()

  let handle_action = function
    | Ac.AppendEntriesRequest args -> Ae.broadcast args
    | Ac.AppendEntriesResponse (res, mvar) -> Lwt_mvar.put mvar res
    | Ac.RequestVotesRequest args -> Rv.broadcast args
    | Ac.RequestVotesResponse (res, mvar) -> Lwt_mvar.put mvar res
    | Ac.ResetElectionTimer -> Lwt_mvar.put reset_election_timeout ()

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
          Some (Some Ev.Timeout)
        in
        let reset_timeout =
          let+ () = Lwt_mvar.take reset_election_timeout in
          Some None
        in
        let get_ae_request =
          let+ ae = Lwt_stream.get t.ae_requests in
          match ae with
          | None -> None
          | Some ae -> Some (Some (Ev.AppendEntriesRequest ae))
        in
        let* event =
          Lwt.pick [ election_timeout; get_ae_request; reset_timeout ]
        in
        match event with
        | None -> Lwt.return_unit
        | Some event -> (
            match event with
            | None ->
                (* received a reset election timout *)
                loop ()
            | Some event ->
                push_event (Some event);
                loop () )
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
            Logs_lwt.info (fun f ->
                f "Received event %s in state %s" (Ev.string event) (S.string s))
          in
          let* s, actions =
            match s with
            | S.Follower s -> Follower.handle s event
            | S.Candidate s -> Lwt.return @@ Candidate.handle s event
            | S.Leader ls -> Lwt.return @@ Leader.handle ls event
          in
          let* () =
            Lwt_list.iter_s
              (fun a ->
                let* () =
                  Logs_lwt.info (fun f ->
                      f "Received action %s and new state %s" (Ac.string a)
                        (S.string s))
                in
                handle_action a)
              actions
          in
          loop s
    in
    loop t.state
end
