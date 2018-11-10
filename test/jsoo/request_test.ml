open Mocha_of_ocaml
module Jo = Jsonrpc_ocaml_jsoo

let tests =
  [ ( "should be able to encode request object to json"
      >:: fun () ->
        let expected_json =
          {|
{"jsonrpc": "2.0", "params": [1,2,3], "method": "sum", "id": "2"}
|}
        in
        let expected = Js._JSON##parse (Js.string expected_json) in
        let request =
          { Jo.Request.id= Some 2L
          ; _method= "sum"
          ; params= Some (Js.Unsafe.coerce @@ Js.(array [|1; 2; 3|])) }
        in
        (* sort key to compare with (=)  *)
        let actual = Jo.Request.to_json request in
        assert_strict_eq actual expected )
  ; ( "should be able to encode a request object for notification to json"
      >:: fun () ->
        let expected_json =
          {|
{"jsonrpc": "2.0", "params": [1,2,3], "method": "sum"}
|}
        in
        let expected = Js._JSON##parse (Js.string expected_json) in
        let request =
          { Jo.Request.id= None
          ; _method= "sum"
          ; params= Some (Js.Unsafe.coerce @@ Js.(array [|1; 2; 3|])) }
        in
        (* sort key to compare with (=)  *)
        let actual = Jo.Request.to_json request in
        assert_strict_eq actual expected ) ]
