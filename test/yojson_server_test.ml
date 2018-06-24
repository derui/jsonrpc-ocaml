open Jsonrpc_ocaml_yojson
module Response = Jsonrpc_ocaml_yojson.Response

open OUnit
let tests = [
  ("should be able to expose method with handler", fun _ ->
      let server = Server.make () in
      let server = Server.expose server ~_method:"test"
          ~handler:(fun _ -> Lwt.return Response.{result = None;
                                                  id = None;
                                                  error = None})
      in
      let expected = Response.{result = None; id = None; error = None} in
      let actual = Server.handle_request server ~request:Request.{
          _method = "test";
          params = None;
          id = None;
        }
      in
      let open Lwt.Infix in
      Lwt_main.run (actual >>= fun actual -> assert_equal expected actual; Lwt.return_unit)
  );
  ("should raise error if call method that does not expose", fun _ ->
      let server = Server.make () in
      let server = Server.expose server ~_method:"test"
          ~handler:(fun _ -> Lwt.return Response.{result = None;
                                                  id = None;
                                                  error = None})
      in
      assert_raises Jsonrpc_ocaml.Types.(Jsonrpc_error Error_code.Method_not_found) (fun _ ->
          Server.handle_request server ~request:Request.{
              _method = "foo";
              params = None;
              id = None;
            })
  );
]
