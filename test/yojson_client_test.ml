module J = Jsonrpc_ocaml
module Jo = Jsonrpc_ocaml_yojson

module type Dummy_raw_client = sig
  include
    J.Client.Raw
    with type json = Yojson.Safe.t
     and module Request = Jo.Request
     and module Response = Jo.Response
     and module Thread = Lwt

  val requests : Jo.Request.t list ref
end

let dummy_raw_client () =
  ( module struct
    type json = Yojson.Safe.t

    module Thread = Lwt
    module Request = Jo.Request
    module Response = Jo.Response

    let requests = ref []

    let call r =
      requests := r :: !requests ;
      Lwt.return Jo.Response.{result = None; id = r.Request.id; error = None}

    let notify r =
      requests := r :: !requests ;
      Lwt.return_unit
  end
  : Dummy_raw_client )

let test_set =
  [ Alcotest_lwt.test_case "should be able to wrap a request with API definition" `Quick
      (fun _ () ->
        let module A = struct
          type json = Yojson.Safe.t
          type params = int list
          type result = int

          let name = "sum"
          let params_to_json v = `List (List.map (fun v -> `Int v) v)
          let result_of_json = function `Int v -> v | _ -> failwith ""
        end in
        let module R = (val dummy_raw_client ()) in
        let module C = Jo.Client.Make (R) in
        let%lwt _ = C.call ~api:(module A) ~params:[1; 2; 3] () in
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
        let module B = struct
          type json = Yojson.Safe.t
          type params = int list
          type result = int

          let name = "sum"
          let params_to_json v = `List (List.map (fun v -> `Int v) v)
          let result_of_json = function `Int v -> v | _ -> failwith ""
        end in
        let module R = (val dummy_raw_client ()) in
        let module C = Jo.Client.Make (R) in
        let%lwt () = C.notify ~api:(module B) ~params:[1; 2; 3] () in
        let req = List.hd !R.requests in
        let expected =
          Jo.Request.{id = None; params = Some (`List [`Int 1; `Int 2; `Int 3]); _method = "sum"}
        in
        Alcotest.(check @@ of_pp Fmt.nop) "expected" expected req ;
        Lwt.return_unit ) ]
