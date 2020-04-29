open Sexplib0.Sexp_conv
open Lwt.Syntax

module Make
    (P : Plog.S)
    (Ae : Append_entries.S with type plog_entry = P.entry)
    (Rv : Request_votes.S)
    (Ev : Event.S
            with type ae_arg := Ae.args
             and type ae_res := Ae.res
             and type rv_arg := Rv.args
             and type rv_res := Rv.res)
    (Ac : Action.S
            with type ae_arg := Ae.args
             and type ae_res := Ae.res
             and type rv_arg := Rv.args
             and type rv_res := Rv.res) =
struct
  type stage = Leader | Candidate | Follower [@@deriving sexp]

  type t = {
    stage : stage; [@default Follower]
    id : int;
    peers : (int * Uri_sexp.t) list;
    log : P.t;
    commit_index : int; [@default 0]
    last_applied : int; [@default 0]
    votes_received : int; [@default 0]
    next_index : int list;
    match_index : int list;
  }
  [@@deriving make, sexp]

  let become_leader (t : t) =
    let* term = P.current_term t.log in
    let+ log = P.set_voted_for t.log None in
    let t =
      {
        t with
        next_index = [];
        match_index = [];
        stage = Leader;
        log;
        votes_received = 0;
      }
    in
    let heartbeat =
      Ac.AppendEntriesRequest
        (Ae.make_args ~term ~leader_id:t.id ~prev_log_index:t.commit_index
           ~prev_log_term:t.commit_index ~leader_commit:t.commit_index ())
    in
    (t, [ heartbeat ])

  let become_candidate (t : t) =
    let* term = P.current_term t.log in
    let term = term + 1 in
    let* log = P.set_current_term t.log term in
    let+ log = P.set_voted_for log (Some t.id) in
    let t = { t with log; stage = Candidate; votes_received = 1 } in
    let rv_args =
      Rv.make_args ~term ~candidate_id:t.id ~last_log_index:0 ~last_log_term:0
    in
    (t, [ Ac.ResetElectionVoteTimer; Ac.RequestVotesRequest rv_args ])

  let handle_timeout (t : t) =
    match t.stage with
    | Leader -> Lwt.return (t, [])
    | _ -> (
        match t.peers with
        | [] ->
            let* current_term = P.current_term t.log in
            let* log = P.set_current_term t.log (current_term + 1) in
            become_leader { t with log }
        | _ -> become_candidate t )

  let handle_vote_timeout (t : t) =
    match t.stage with
    | Follower | Leader -> Lwt.return (t, [])
    | Candidate -> become_candidate t

  let handle_send_heartbeat (t : t) =
    match t.stage with
    | Follower | Candidate -> Lwt.return (t, [])
    | Leader ->
        let+ term = P.current_term t.log in
        let ae_args =
          Ae.make_args ~term ~leader_id:t.id ~prev_log_index:t.last_applied
            ~prev_log_term:term ~leader_commit:t.commit_index ()
        in
        (t, [ Ac.AppendEntriesRequest ae_args ])

  let handle_append_entries_request (t : t)
      ((req, mvar) : Ae.args * Ae.res Lwt_mvar.t) =
    (* if leader_commit_index  > last_applied, increment last_applied, apply log[last_applied] to state machine *)
    let* t =
      let* current_term = P.current_term t.log in
      if req.term > current_term then
        let+ log = P.set_current_term t.log (current_term + 1) in
        { t with stage = Follower; log }
      else Lwt.return t
    in
    let* current_term = P.current_term t.log in
    if req.term < current_term then
      let response = Ae.make_res ~term:current_term ~success:false in
      Lwt.return (t, [ Ac.AppendEntriesResponse (response, mvar) ])
    else
      let* prev_log_index_entry = P.get t.log req.prev_log_index in
      match prev_log_index_entry with
      | None ->
          let response = Ae.make_res ~term:current_term ~success:false in
          Lwt.return (t, [ Ac.AppendEntriesResponse (response, mvar) ])
      | Some entry ->
          if entry.term <> req.prev_log_term then
            let response = Ae.make_res ~term:current_term ~success:false in
            Lwt.return (t, [ Ac.AppendEntriesResponse (response, mvar) ])
          else
            let* t, _ =
              Lwt_list.fold_left_s
                (fun (t, i) (entry : Ae.plog_entry) ->
                  let* e = P.get t.log i in
                  match e with
                  | None -> Lwt.return (t, i + 1)
                  | Some e ->
                      if e.term = entry.term then Lwt.return (t, i + 1)
                      else
                        let+ log = P.delete_since t.log i in
                        ({ t with log }, i + 1))
                (t, req.prev_log_index + 1)
                req.entries
            in
            let* t, _ =
              Lwt_list.fold_left_s
                (fun (t, i) entry ->
                  let+ log = P.insert t.log i entry in
                  ({ t with log }, i + 1))
                (t, req.prev_log_index + 1)
                req.entries
            in
            let response = Ae.make_res ~term:current_term ~success:true in
            if req.leader_commit > t.commit_index then
              let t =
                {
                  t with
                  commit_index =
                    min req.leader_commit
                      (req.prev_log_index + List.length req.entries);
                }
              in
              Lwt.return (t, [ Ac.AppendEntriesResponse (response, mvar) ])
            else Lwt.return (t, [ Ac.AppendEntriesResponse (response, mvar) ])

  let handle_append_entries_response (t : t) (res : Ae.res) =
    let* current_term = P.current_term t.log in
    if res.term > current_term then
      let+ t =
        let* log = P.set_current_term t.log res.term in
        let+ log = P.set_voted_for log None in
        { t with log; stage = Follower }
      in
      (t, [])
    else Lwt.return (t, [])

  let handle_request_votes_request (t : t)
      ((req, mvar) : Rv.args * Rv.res Lwt_mvar.t) =
    let* current_term = P.current_term t.log in
    if req.term > current_term then
      let+ t =
        let+ log = P.set_current_term t.log req.term in
        { t with log }
      in
      (t, [])
    else if req.term = current_term then
      let* voted_for = P.voted_for t.log in
      match voted_for with
      | None ->
          let+ t =
            let+ log = P.set_voted_for t.log (Some req.candidate_id) in
            { t with log }
          in
          let resp = Rv.make_res ~term:current_term ~vote_granted:true in
          (t, [ Ac.RequestVotesResponse (resp, mvar) ])
      | Some i when i = req.candidate_id ->
          let resp = Rv.make_res ~term:current_term ~vote_granted:true in
          Lwt.return (t, [ Ac.RequestVotesResponse (resp, mvar) ])
      | Some _ ->
          let resp = Rv.make_res ~term:current_term ~vote_granted:false in
          Lwt.return (t, [ Ac.RequestVotesResponse (resp, mvar) ])
    else
      let resp = Rv.make_res ~term:current_term ~vote_granted:false in
      Lwt.return (t, [ Ac.RequestVotesResponse (resp, mvar) ])

  let handle_request_votes_response (t : t) (res : Rv.res) =
    let* current_term = P.current_term t.log in
    if res.term > current_term then
      let+ t =
        let* log = P.set_current_term t.log res.term in
        let+ log = P.set_voted_for log None in
        { t with log }
      in
      (t, [])
    else
      let votes = t.votes_received in
      if res.term = current_term && res.vote_granted then
        let vote_count = votes + 1 in
        if 2 * vote_count > List.length t.peers + 1 then become_leader t
        else
          let t = { t with votes_received = vote_count } in
          Lwt.return (t, [])
      else Lwt.return (t, [])

  let handle t event =
    match event with
    | Ev.ElectionTimeout -> handle_timeout t
    | Ev.ElectionVoteTimeout -> handle_vote_timeout t
    | Ev.SendHeartbeat -> handle_send_heartbeat t
    | Ev.AppendEntriesRequest req -> handle_append_entries_request t req
    | Ev.AppendEntriesResponse res -> handle_append_entries_response t res
    | Ev.RequestVotesRequest req -> handle_request_votes_request t req
    | Ev.RequestVotesResponse res -> handle_request_votes_response t res
end
