open Mirage

let port =
  let doc =
    Key.Arg.info ~doc:"The port on which to listen for incoming connections."
      [ "port" ]
  in
  Key.(create "port" Arg.(required int doc))

let id =
  let doc = Key.Arg.info ~doc:"ID of this raft server" [ "id" ] in
  Key.(create "id" Arg.(required int doc))

let peer_uris =
  let doc = Key.Arg.info ~doc:"List of peer uris" [ "peer-uris" ] in
  Key.(create "peer-uris" Arg.(opt (list string) [] doc))

let peer_ids =
  let doc = Key.Arg.info ~doc:"List of peer ids" [ "peer-ids" ] in
  Key.(create "peer-ids" Arg.(opt (list int) [] doc))

let debug =
  let doc = Key.Arg.info ~doc:"Enable debug logging" [ "debug" ] in
  Key.(create "debug" Arg.(flag doc))

let timeout_lower =
  let doc =
    Key.Arg.info ~doc:"Lower bound for election timeout, inclusive"
      [ "timeout-lower" ]
  in
  Key.(create "timeout-lower" Arg.(opt int 150 doc))

let timeout_upper =
  let doc =
    Key.Arg.info ~doc:"Upper bound for election timeout, exclusive"
      [ "timeout-upper" ]
  in
  Key.(create "timeout-upper" Arg.(opt int 301 doc))

let heartbeat =
  let doc =
    Key.Arg.info ~doc:"Duration between heartbeats from leader" [ "heartbeat" ]
  in
  Key.(create "heartbeat" Arg.(opt int 100 doc))

let () =
  let main =
    let packages =
      [
        package "duration";
        package "mirage-raft";
        package "ppx_sexp_conv";
        package "cohttp-mirage";
        package "ppx_deriving.make";
      ]
    in
    foreign
      ~keys:
        [
          Key.abstract port;
          Key.abstract id;
          Key.abstract peer_uris;
          Key.abstract peer_ids;
          Key.abstract debug;
          Key.abstract timeout_lower;
          Key.abstract timeout_upper;
          Key.abstract heartbeat;
        ]
      ~packages "Unikernel.Raft"
      (stackv4 @-> conduit @-> time @-> random @-> pclock @-> job)
  in
  let stack = generic_stackv4 default_network in
  let conduit = conduit_direct stack in
  register "raft"
    [
      main $ stack $ conduit $ default_time $ default_random
      $ default_posix_clock;
    ]
