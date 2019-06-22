module J = Jsonrpc
module Jo = Jsonrpc_yojson
module Jyc = Jo.Client.Make (Lwt)

module type Dummy_raw_client = sig
  include Jyc.S

  val requests : Jo.Request.t list ref
end

let dummy_raw_client () =
  ( module struct
    type json = Yojson.Safe.t

    let requests = ref []

    let call ~api ?params () =
      let r = Jyc.Helper.setup_request ~api ~params in
      requests := r :: !requests ;
      Lwt.return_ok None

    let notify ~api ?params () =
      let r = Jyc.Helper.setup_request ~api ~params in
      requests := r :: !requests ;
      Lwt.return_unit
  end
  : Dummy_raw_client )

let test_set =
  [ Alcotest_lwt.test_case "should be able to wrap a request with API definition" `Quick
      (fun _ () ->
        let module R = (val dummy_raw_client ()) in
        let%lwt _ =
          R.call
            ~api:
              { Jyc._method = "sum"
              ; params_to_json = (fun v -> `List (List.map (fun v -> `Int v) v))
              ; result_of_json = (function `Int v -> v | _ -> failwith "") }
            ~params:[1; 2; 3] ()
        in
        let req = List.hd !R.requests in
        let expected =
          Jo.Request.
            { id = req.Jo.Request.id
            ; params = Some (`List [`Int 1; `Int 2; `Int 3])
            ; _method = "sum" }
        in
        Alcotest.(check @@ of_pp Fmt.nop) "expected" expected req ;
        Lwt.return_unit )
  ; Alcotest_lwt.test_case "should be able to wrap a notification with API definition" `Quick
      (fun _ () ->
        let module R = (val dummy_raw_client ()) in
        let%lwt () =
          R.notify
            ~api:
              { Jyc._method = "sum"
              ; params_to_json = (fun v -> `List (List.map (fun v -> `Int v) v))
              ; result_of_json = (function `Int v -> v | _ -> failwith "") }
            ~params:[1; 2; 3] ()
        in
        let req = List.hd !R.requests in
        let expected =
          Jo.Request.{id = None; params = Some (`List [`Int 1; `Int 2; `Int 3]); _method = "sum"}
        in
        Alcotest.(check @@ of_pp Fmt.nop) "expected" expected req ;
        Lwt.return_unit ) ]
