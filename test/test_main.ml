open OUnit2

let () =
  let converter_specs = List.map (fun (desc, spec) ->
      desc >:: (fun _ -> assert_bool "desc" @@ spec ())
    ) Jsonrpc_ocaml.Types.Test.tests
  in
  let suite =
    "JSON RPC for OCaml" >::: converter_specs
  in

  run_test_tt_main suite
