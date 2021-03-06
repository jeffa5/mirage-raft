open Lwt.Syntax

let src = Logs.Src.create "raft"

module L = (val Logs_lwt.src_log src)

module Make
    (Time : Mirage_time.S)
    (M : Mirage_clock.MCLOCK)
    (C : Command.S)
    (P : Plog.S with type command := C.input)
    (Ae : Append_entries.S with type plog_entry = P.entry)
    (Rv : Request_vote.S with type address = Ae.address) =
struct
  module Ev = Event.Make (Ae) (Rv) (C)
  module Ac = Action.Make (Ae) (Rv) (C)
  module S = State.Make (C) (P) (Ae) (Rv) (Ev) (Ac)
  module Rng = Mirage_crypto_rng_mirage.Make (Time) (M)

  type t = {
    state : S.t;
    timeout : int * int;
    heartbeat : int;
    peers : (int * Ae.address) list;
    ae_requests : (Ae.args * Ae.res Lwt_mvar.t) Lwt_stream.t;
    ae_responses : (Ae.args * Ae.res) Lwt_stream.t;
    rv_requests : (Rv.args * Rv.res Lwt_mvar.t) Lwt_stream.t;
    rv_responses : Rv.res Lwt_stream.t;
    commands : (C.input * C.output Lwt_mvar.t) Lwt_stream.t;
  }

  (** timeout is the lower and upper bounds for the election timeout, [lower,upper), in ms
   *  heartbeat is the duration on which to repeat the heartbeat, in ms *)
  let v ?(timeout_lower = 150) ?(timeout_upper = 301) ?(heartbeat = 50)
      ae_requests ae_responses rv_requests rv_responses commands id peers =
    let+ state =
      let+ log = P.v () in
      let peers = List.map (fun (id, _) -> S.make_peer ~id ()) peers in
      S.make ~id ~peers ~log ()
    in
    {
      state;
      timeout = (timeout_lower, timeout_upper);
      heartbeat;
      peers;
      ae_requests;
      ae_responses;
      rv_requests;
      rv_responses;
      commands;
    }

  let reset_election_timeout = Lwt_mvar.create_empty ()

  let handle_action (t : t) = function
    | Ac.AppendEntriesRequest (id, args) ->
        List.iter
          (fun (i, addr) ->
            if i = id then Lwt.async (fun () -> Ae.send addr args))
          t.peers
        |> Lwt.return
    | Ac.AppendEntriesResponse (res, mvar) -> Lwt_mvar.put mvar res
    | Ac.RequestVoteRequest (id, args) ->
        List.iter
          (fun (i, addr) ->
            if i = id then Lwt.async (fun () -> Rv.send addr args))
          t.peers
        |> Lwt.return
    | Ac.RequestVoteResponse (res, mvar) -> Lwt_mvar.put mvar res
    | Ac.ResetElectionTimeout -> Lwt_mvar.put reset_election_timeout ()
    | Ac.CommandResponse (res, mvar) -> Lwt_mvar.put mvar res

  let id_tag =
    Logs.Tag.def "id" ~doc:"id" (fun f a ->
        Format.fprintf f "%s" (string_of_int a))

  let handle (t : t) =
    let* () =
      L.info (fun f ->
          f "Starting raft" ~tags:Logs.Tag.(empty |> add id_tag t.state.id))
    in
    (* events is the stream where all events come through, i.e.
     * Timeouts
     * Append Entries requests
     * Request Votes requests *)
    let events, push_event = Lwt_stream.create () in

    (* receive the call to append entries or a timeout and push the event to events *)
    let append_entries_requests () =
      let rec loop () =
        let* ae = Lwt_stream.get t.ae_requests in
        match ae with
        | None -> Lwt.return_unit
        | Some ae ->
            push_event (Some (Ev.AppendEntriesRequest ae));
            loop ()
      in
      loop ()
    in
    Lwt.async append_entries_requests;

    let election_timeout () =
      let lower, upper = t.timeout in
      let rec loop () =
        let timeout =
          let+ () =
            Time.sleep_ns
              (Duration.of_ms
                 (lower + Randomconv.int ~bound:(upper - lower) Rng.generate))
          in
          Some (Some Ev.ElectionTimeout)
        in
        let reset_timeout =
          let+ () = Lwt_mvar.take reset_election_timeout in
          Some None
        in
        let* event = Lwt.pick [ timeout; reset_timeout ] in
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
    Lwt.async election_timeout;

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
            push_event (Some (Ev.RequestVoteRequest rv));
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
            push_event (Some (Ev.RequestVoteResponse rv));
            loop ()
      in
      loop ()
    in
    Lwt.async request_votes_responses;

    (* ticker for leader to send heartbeats, can be ignored by others *)
    let heartbeat_ticker () =
      let rec loop () =
        let* () = Time.sleep_ns (Duration.of_ms t.heartbeat) in
        push_event (Some Ev.SendHeartbeat);
        loop ()
      in
      loop ()
    in
    Lwt.async heartbeat_ticker;

    let command_inputs () =
      let rec loop () =
        let* command = Lwt_stream.get t.commands in
        match command with
        | None -> Lwt.return_unit
        | Some command ->
            push_event (Some (Ev.CommandReceived command));
            loop ()
      in
      loop ()
    in
    Lwt.async command_inputs;

    let state_tag =
      Logs.Tag.def "state" ~doc:"Current state" (fun f s ->
          Sexplib0.Sexp.pp f (S.sexp_of_t s))
    in
    let event_tag =
      Logs.Tag.def "event" ~doc:"Event received" (fun f e ->
          Sexplib0.Sexp.pp f (Ev.sexp_of_t e))
    in
    let action_tag =
      Logs.Tag.def "action" ~doc:"Action processing" (fun f a ->
          Sexplib0.Sexp.pp f (Ac.sexp_of_t a))
    in

    let rec loop (last : S.t) (s : S.t) =
      let* () =
        if last.stage <> s.stage then
          L.info (fun f ->
              f "State %s"
                (Sexplib0.Sexp_conv.string_of_sexp @@ S.sexp_of_stage s.stage)
                ~tags:Logs.Tag.(empty |> add id_tag s.id))
        else Lwt.return_unit
      in
      let* event = Lwt_stream.get events in
      match event with
      | None -> Lwt.return_unit
      | Some event ->
          let* () =
            L.debug (fun f ->
                f "-> Event"
                  ~tags:
                    Logs.Tag.(
                      empty |> add event_tag event |> add state_tag s
                      |> add id_tag s.id))
          in
          let* s', actions = S.handle s event in
          let* () =
            Lwt_list.iter_s
              (fun a ->
                let* () =
                  L.debug (fun f ->
                      f "<- Action"
                        ~tags:
                          Logs.Tag.(
                            empty |> add action_tag a |> add state_tag s'
                            |> add id_tag s'.id))
                in
                handle_action t a)
              actions
          in
          loop s s'
    in
    loop t.state t.state
end
