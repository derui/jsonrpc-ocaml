open Mocha_of_ocaml
open Jsonrpc_ocaml_jsoo.Response
module J = Jsonrpc_ocaml

let tests =
  [ ( "should be able to parse response object from json"
      >:: fun () ->
        let json = {|
{"jsonrpc": "2.0", "result": 19, "id": "1"}
|} in
        let expected =
          { id= Some 1L
          ; result= Some (Js.Unsafe.coerce (Js.number_of_float 19.0))
          ; error= None }
        in
        assert_strict_eq (Js._JSON##parse (Js.string json)) (to_json expected) )
  ; ( "should be able to parse error response object from json"
      >:: fun () ->
        let json =
          {|
{"jsonrpc": "2.0", "error": {"code": -32700, "message": "Parse error"}, "id": "1"}
|}
        in
        let expected =
          { id= Some 1L
          ; result= None
          ; error= Some Error.{code= J.Types.Error_code.Parse_error; data= None} }
        in
        assert_strict_eq (Js._JSON##parse (Js.string json)) (to_json expected) )
  ; ( "should be able to parse error response object with data from json"
      >:: fun () ->
        let json =
          {|
{"jsonrpc": "2.0", "error": {"code": -32700, "message": "Parse error", "data": "detail"}, "id": "1"}
|}
        in
        let expected =
          { id= Some 1L
          ; error=
              Some
                Error.
                  { code= J.Types.Error_code.Parse_error
                  ; data= Some (Js.Unsafe.coerce @@ Js.string "detail") }
          ; result= None }
        in
        assert_strict_eq (Js._JSON##parse (Js.string json)) (to_json expected) ) ]
