open Mocha_of_ocaml

let _ =
  let jsoo_response_specs = List.map (fun (desc, spec) ->
      desc >:: (fun _ -> spec ())
    ) Jsonrpc_ocaml_jsoo.Response.Test.tests
  and jsoo_client_specs = List.map (fun (desc, spec) ->
      desc >:: (fun _ -> spec ())
    ) Jsonrpc_ocaml_jsoo.Client.Test.tests
  and jsoo_request_specs = List.map (fun (desc, spec) ->
      desc >:: (fun _ -> spec ())
    ) Jsonrpc_ocaml_jsoo.Request.Test.tests
  in
  "JSON-RPC implemented by Js_of_ocaml" >::: (jsoo_request_specs @ jsoo_client_specs @ jsoo_response_specs)
