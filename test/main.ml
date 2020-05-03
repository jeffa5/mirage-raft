let () = Alcotest.run "Raft" [ ("qcheck states", Qcheck.tests) ]
