module J = Jsonrpc
module Jo = Jsonrpc_yojson

let test_set =
  [ ( "should be able to make an error object from code"
    , `Quick
    , fun () ->
        let error = Jo.Error.make J.Types.Error_code.Method_not_found in
        Alcotest.(check @@ int) "code" J.Types.Error_code.(to_int Method_not_found) error.code )
  ; ( "should be able to convert to code"
    , `Quick
    , fun () ->
        let error = Jo.Error.make J.Types.Error_code.Method_not_found in
        let code = Jo.Error.to_error_code error in
        Alcotest.(check @@ of_pp Fmt.nop) "code" J.Types.Error_code.(Method_not_found) code )
  ; ( "should be able to convert to json"
    , `Quick
    , fun () ->
        let expected = {|
{"code": -32601, "message": "Method not found"}
|} in
        let expected = Yojson.Safe.from_string expected |> Yojson.Safe.sort in
        let error =
          Jo.Error.make J.Types.Error_code.Method_not_found |> Jo.Error.to_json |> Yojson.Safe.sort
        in
        Alcotest.(check @@ of_pp Fmt.nop) "code" expected error ) ]
