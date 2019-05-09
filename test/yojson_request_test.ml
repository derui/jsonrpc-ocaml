module J = Jsonrpc_ocaml
module Jo = Jsonrpc_ocaml_yojson
open Jo.Request

let test_set =
  [ ( "should be able to encode request object to json"
    , `Quick
    , fun () ->
        let expected_json =
          {|
{"jsonrpc": "2.0", "params": [1,2,3], "method": "sum", "id": "2"}
|}
        in
        let expected_json = Yojson.Safe.from_string expected_json in
        let request =
          { id= Some 2L
          ; _method= "sum"
          ; params= Some (`List [`Int 1; `Int 2; `Int 3]) }
        in
        (* sort key to compare with (=)  *)
        let actual = Yojson.Safe.sort @@ to_json request
        and expected = Yojson.Safe.sort expected_json in
        Alcotest.(check @@ of_pp Fmt.nop) "expected" actual expected )
  ; ( "should be able to encode a request object for notification to json"
    , `Quick
    , fun () ->
        let expected_json =
          {|
{"jsonrpc": "2.0", "params": [1,2,3], "method": "sum"}
|}
        in
        let expected_json = Yojson.Safe.from_string expected_json in
        let request =
          { id= None
          ; _method= "sum"
          ; params= Some (`List [`Int 1; `Int 2; `Int 3]) }
        in
        (* sort key to compare with (=)  *)
        let actual = Yojson.Safe.sort @@ to_json request
        and expected = Yojson.Safe.sort expected_json in
        Alcotest.(check @@ of_pp Fmt.nop) "expected" actual expected )
  ; ( "should be able to handle object that omits params"
    , `Quick
    , fun () ->
        let expected_json =
          {|
{"jsonrpc": "2.0", "method": "sum", "id": "2"}
|}
        in
        let expected_json = Yojson.Safe.from_string expected_json in
        let request = {id= Some 2L; _method= "sum"; params= None} in
        (* sort key to compare with (=)  *)
        let actual = Yojson.Safe.sort @@ to_json request
        and expected = Yojson.Safe.sort expected_json in
        Alcotest.(check @@ of_pp Fmt.nop) "expected" actual expected ) ]
