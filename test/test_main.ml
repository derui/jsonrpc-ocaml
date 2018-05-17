open OUnit2

let () =
  let response_specs = List.map (fun (desc, spec) ->
      desc >:: (fun _ -> assert_bool "desc" @@ spec ())
    ) Jsonrpc_ocaml_yojson.Response.Test.tests
  and client_specs = List.map (fun (desc, spec) ->
      desc >:: (fun _ -> assert_bool "desc" @@ spec ())
    ) Jsonrpc_ocaml_yojson.Client.Test.tests
  and request_specs = List.map (fun (desc, spec) ->
      desc >:: (fun _ -> assert_bool "desc" @@ spec ())
    ) Jsonrpc_ocaml_yojson.Request.Test.tests
  in
  let suite =
    "JSON RPC for OCaml" >::: (response_specs @ client_specs @ request_specs)
  in

  run_test_tt_main suite
