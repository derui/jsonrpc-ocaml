open Jsonrpc_ocaml_yojson

let test_set =
  [ Alcotest_lwt.test_case "should be able to expose method with handler" `Quick
      (fun _ () ->
         let server = Server.make () in
         let server =
           Server.expose server ~_method:"test" ~handler:(fun _ ->
               Lwt.return Response.{result= None; id= None; error= None} )
         in
         let expected = Response.{result= None; id= None; error= None} in
         let actual =
           Server.handle_request server
             ~request:{Request._method= "test"; params= None; id= None}
         in
         let open Lwt.Infix in
         actual
         >>= fun actual ->
         Alcotest.(check @@ of_pp Fmt.nop) "expect" expected actual ;
         Lwt.return_unit )
  ; Alcotest_lwt.test_case
      "should return response as error if call method that does not expose"
      `Quick (fun _ () ->
          let server = Server.make () in
          let server =
            Server.expose server ~_method:"test" ~handler:(fun _ ->
                Lwt.return {Response.result= None; id= None; error= None} )
          in
          let actual =
            Server.handle_request server
              ~request:Request.{_method= "foo"; params= None; id= None}
          in
          let expected =
            { Response.result= None
            ; id= None
            ; error=
                Some {Error.code= Types.Error_code.Method_not_found; data= None}
            }
          in
          let open Lwt.Infix in
          actual
          >>= fun actual ->
          Alcotest.(check @@ of_pp Fmt.nop) "expect" expected actual ;
          Lwt.return_unit ) ]
