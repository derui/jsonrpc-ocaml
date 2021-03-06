module J = Jsonrpc
module Jy = Jsonrpc_yojson
open Jy.Response

let test_set =
  [ ( "should be able to parse response object from json"
    , `Quick
    , fun () ->
        let json = {|
{"jsonrpc": "2.0", "result": 19, "id": "1"}
|} in
        let expected = {id = Some 1L; result = Some (`Int 19); error = None} in
        let actual = of_json @@ Yojson.Safe.from_string json in
        Alcotest.(check @@ of_pp Fmt.nop) "expected" actual @@ Ok expected )
  ; ( "should be able to parse error response object from json"
    , `Quick
    , fun () ->
        let json =
          {|
{"jsonrpc": "2.0", "error": {"code": -32700, "message": "Parse error"}, "id": "1"}
|}
        in
        let expected =
          {id = Some 1L; result = None; error = Some Jy.Error.(make J.Types.Error_code.Parse_error)}
        in
        let actual = of_json @@ Yojson.Safe.from_string json in
        Alcotest.(check @@ of_pp Fmt.nop) "expected" actual @@ Ok expected )
  ; ( "should be able to parse error response object with data from json"
    , `Quick
    , fun () ->
        let json =
          {|
{"jsonrpc": "2.0", "error": {"code": -32700, "message": "Parse error", "data": "detail"}, "id": "1"}
|}
        in
        let expected =
          { id = Some 1L
          ; error = Some Jy.Error.(make ~data:(`String "detail") J.Types.Error_code.Parse_error)
          ; result = None }
        in
        let actual = of_json @@ Yojson.Safe.from_string json in
        Alcotest.(check @@ of_pp Fmt.nop) "expected" actual @@ Ok expected ) ]
