let () =
  Alcotest.run "Yojson portings"
    [ ("server", Yojson_server_test.test_set)
    ; ("client", Yojson_client_test.test_set) ]
