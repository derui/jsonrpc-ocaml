let () =
  Alcotest.run "Yojson portings"
    [ ("server", Yojson_server_test.test_set)
    ; ("client", Yojson_client_test.test_set)
    ; ("request", Yojson_request_test.test_set)
    ; ("response", Yojson_response_test.test_set) ]
