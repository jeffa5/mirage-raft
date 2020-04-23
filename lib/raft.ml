(** main initialisation *)
let ( let* ) = Lwt.bind

let ( let+ ) a b = Lwt.map b a

module Make
    (Time : Mirage_time.S)
    (P : Plog.S)
    (Ae : Append_entries.S with type plog_entry := P.entry)
    (Rv : Request_votes.S) =
struct
  module S = State.Make (P)
  module Leader = Leader.Make (P) (S) (Ae)
  module Candidate = Candidate.Make (P) (S)
  module Follower = Follower.Make (P) (S) (Ae) (Rv)

  let event_to_string = function
    | `Timeout -> "timeout"
    | `SendHeartbeat -> "sendheartbeat"
    | `AppendEntries (_, _) -> "appendentries"
    | `RequestVotes (_, _) -> "requestvotes"

  type t = {
    id : int;
    peer_ids : int list;
    state : S.t;
    ae_stream : (Ae.args * Ae.res Lwt_mvar.t) Lwt_stream.t;
    rv_stream : (Rv.args * Rv.res Lwt_mvar.t) Lwt_stream.t;
  }

  let v ?(current_term = 0) ?(voted_for = None) ?(log = P.empty) ae_stream
      rv_stream id peer_ids =
    let initial_state : S.state =
      let persistent : S.persistent = { current_term; voted_for; log } in
      let volatile : S.volatile = { commit_index = 0; last_applied = 0 } in
      { persistent; volatile }
    in
    { id; peer_ids; state = S.Follower initial_state; ae_stream; rv_stream }

  let handle (t : t) =
    let* () = Logs_lwt.info (fun f -> f "Starting raft") in
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
          Some `Timeout
        in
        let get_ae =
          let+ ae = Lwt_stream.get t.ae_stream in
          match ae with None -> None | Some ae -> Some (`AppendEntries ae)
        in
        let* event = Lwt.pick [ election_timeout; get_ae ] in
        match event with
        | None -> Lwt.return_unit
        | Some event ->
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
          let* () =
            Logs_lwt.info (fun f ->
                f "Received event %s" (event_to_string event))
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
