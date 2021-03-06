open Jsonrpc_yojson

module Server =
  Server.Make
    (struct
      type t = Yojson.Safe.t
    end)
    (struct
      let serialize v = match Request.of_json v with Ok v -> Ok v | Error _ -> Error "error"
    end)
    (struct
      let deserialize v = Response.to_json v
    end)

let test_set =
  [
    Alcotest_lwt.test_case "should be able to expose method with handler" `Quick (fun _ () ->
        let server = Server.make () in
        let server = Server.expose server ~_method:"test" ~handler:(fun _ -> Lwt.return_ok None) in
        let expected = Response.(to_json { result = None; id = None; error = None }) in
        let actual =
          Server.handle_request server
            ~request:Request.(to_json { _method = "test"; params = None; id = None })
        in
        let open Lwt.Infix in
        actual >>= fun actual ->
        Alcotest.(check @@ of_pp Fmt.nop) "expect" expected actual;
        Lwt.return_unit);
    Alcotest_lwt.test_case "should return response as error if call method that does not expose"
      `Quick (fun _ () ->
        let module J = Jsonrpc in
        let server = Server.make () in
        let server = Server.expose server ~_method:"test" ~handler:(fun _ -> Lwt.return_ok None) in
        let actual =
          Server.handle_request server
            ~request:Request.(to_json { _method = "foo"; params = None; id = None })
        in
        let expected =
          Response.(
            to_json
              {
                result = None;
                id = None;
                error = Some Error.(make J.Types.Error_code.Method_not_found);
              })
        in
        let open Lwt.Infix in
        actual >>= fun actual ->
        Alcotest.(check @@ of_pp Fmt.nop) "expect" expected actual;
        Lwt.return_unit);
  ]
