open Sexplib0.Sexp_conv
open Lwt.Syntax

module Make
    (P : Plog.S)
    (Ae : Append_entries.S with type plog_entry := P.entry)
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

  let handle_timeout (t : t) =
    match t.peers with
    | [] ->
        (* straight to leader *)
        let* current_term = P.current_term t.log in
        let* log = P.set_current_term t.log (current_term + 1) in
        become_leader { t with log }
    | _ ->
        let* current_term = P.current_term t.log in
        let new_term = current_term + 1 in
        let* log = P.set_current_term t.log new_term in
        let+ log = P.set_voted_for log (Some t.id) in
        let t = { t with log; stage = Candidate; votes_received = 1 } in
        let rv_args =
          Rv.make_args ~term:new_term ~candidate_id:t.id ~last_log_index:0
            ~last_log_term:0
        in
        (t, [ Ac.ResetElectionTimer; Ac.RequestVotesRequest rv_args ])

  let handle_send_heartbeat (t : t) =
    match t.stage with
    | Follower | Candidate ->
        (* only leaders send heartbeats *) Lwt.return (t, [])
    | Leader ->
        let+ current_term = P.current_term t.log in
        let ae_args =
          Ae.make_args ~term:current_term ~leader_id:t.id
            ~prev_log_index:t.last_applied ~prev_log_term:current_term
            ~leader_commit:t.commit_index ()
        in
        (t, [ Ac.AppendEntriesRequest ae_args ])

  let handle_append_entries_request (t : t)
      ((ae, m) : Ae.args * Ae.res Lwt_mvar.t) =
    let* resp =
      let* current_term = P.current_term t.log in
      if (* reply false if term < currentTerm *)
         ae.term < current_term then
        Lwt.return ({ term = current_term; success = false } : Ae.res)
      else
        let* entry = P.get t.log ae.prev_log_index in
        match entry with
        | None -> Lwt.return ({ term = current_term; success = false } : Ae.res)
        | Some entry ->
            if entry.term <> ae.prev_log_term then
              (* reply false if log  doesn't contain  an entry  at prev_log_index whose term matches prev_log_term*)
              Lwt.return ({ term = current_term; success = false } : Ae.res)
            else
              let* _, _l =
                Lwt_list.fold_left_s
                  (fun (i, l) entry ->
                    let entry : P.entry = entry in
                    let* existing = P.get l i in
                    (* if an existing entry conflicts with a new one (same index but different terms), delete the existing entry and all that follow it*)
                    let* l =
                      match existing with
                      | None -> Lwt.return l
                      | Some existing ->
                          if existing.term <> entry.term then P.delete_since l i
                          else Lwt.return l
                    in

                    (* append any new entries not already in the log *)
                    let* l = P.insert l i entry in
                    Lwt.return (i + 1, l))
                  (ae.prev_log_index + 1, t.log)
                  ae.entries
              in
              Lwt.return ({ term = current_term; success = false } : Ae.res)
    in
    (* if leader_commit > commit_index, set commit_index=min(leader_commit, index of last new entry) *)
    let t =
      if ae.leader_commit > t.commit_index then
        { t with commit_index = min ae.leader_commit t.last_applied }
      else t
    in
    Lwt.return
      ({ t with stage = Follower }, [ Ac.AppendEntriesResponse (resp, m) ])

  let handle_append_entries_response (t : t) (res : Ae.res) =
    let* current_term = P.current_term t.log in
    if res.term > current_term then
      let+ t =
        let* log = P.set_current_term t.log res.term in
        let+ log = P.set_voted_for log None in
        { t with log; stage = Follower }
      in
      (t, [])
    else (* ignore this response *) Lwt.return (t, [])

  let handle_request_votes_request (t : t)
      ((req, mvar) : Rv.args * Rv.res Lwt_mvar.t) =
    let* current_term = P.current_term t.log in
    if req.term > current_term then
      let+ t =
        let* log = P.set_current_term t.log req.term in
        let+ log = P.set_voted_for log None in
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
          (t, [ Ac.ResetElectionTimer; Ac.RequestVotesResponse (resp, mvar) ])
      | Some i when i = req.candidate_id ->
          let resp = Rv.make_res ~term:current_term ~vote_granted:true in
          Lwt.return
            (t, [ Ac.ResetElectionTimer; Ac.RequestVotesResponse (resp, mvar) ])
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
        (* need to update the vote count *)
        let vote_count = votes + 1 in
        if 2 * vote_count > List.length t.peers + 1 then
          (* we have a majority so can become a leader *)
          (* become leader so send out initial heartbeat *)
          become_leader t
        else
          let t = { t with votes_received = vote_count } in
          Lwt.return (t, [])
      else
        (* ignore the vote since it is for an incorrect term, maybe a previous vote *)
        Lwt.return (t, [])

  let handle t event =
    match event with
    | Ev.ElectionTimeout -> handle_timeout t
    | Ev.SendHeartbeat -> handle_send_heartbeat t
    | Ev.AppendEntriesRequest req -> handle_append_entries_request t req
    | Ev.AppendEntriesResponse res -> handle_append_entries_response t res
    | Ev.RequestVotesRequest req -> handle_request_votes_request t req
    | Ev.RequestVotesResponse res -> handle_request_votes_response t res
end
