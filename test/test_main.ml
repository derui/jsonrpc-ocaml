open OUnit2

let () =
  let response_specs = List.map (fun (desc, spec) ->
      desc >:: (fun _ -> spec ())
    ) Jsonrpc_ocaml_yojson.Response.Test.tests
  and client_specs = List.map (fun (desc, spec) ->
      desc >:: (fun _ -> spec ())
    ) Jsonrpc_ocaml_yojson.Client.Test.tests
  and request_specs = List.map (fun (desc, spec) ->
      desc >:: (fun _ -> spec ())
    ) Jsonrpc_ocaml_yojson.Request.Test.tests
  and server_specs = List.map (fun (desc, spec) ->
      desc >:: (fun _ -> spec ())
    ) Jsonrpc_ocaml_yojson.Server.Test.tests
  in
  let suite =
    "JSON RPC for OCaml" >::: (response_specs @ client_specs @ request_specs @ server_specs)
  in

  run_test_tt_main suite
