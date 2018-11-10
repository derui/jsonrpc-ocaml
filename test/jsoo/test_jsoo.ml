open Mocha_of_ocaml

let tests = Client_test.tests @ Request_test.tests @ Response_test.tests

let () = "JSON-RPC implemented by Js_of_ocaml" >::: tests
