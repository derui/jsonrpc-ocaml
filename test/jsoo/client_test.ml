open Mocha_of_ocaml
open Mocha_of_ocaml_async
module J = Jsonrpc_ocaml
module Jo = Jsonrpc_ocaml_jsoo

module type Dummy_rpc = sig
  include
    J.Rpc.S
    with type json = < > Js.t
     and module Request = Jo.Request
     and module Response = Jo.Response
     and module Thread = Lwt

  val requests : Jo.Request.t list ref
end

let dummy_rpc () =
  ( module struct
    type json = < > Js.t

    module Thread = Lwt
    module Request = Jo.Request
    module Response = Jo.Response

    let requests = ref []

    let call r =
      requests := r :: !requests ;
      Lwt.return Jo.Response.{result= None; id= r.Request.id; error= None}

    let notify r =
      requests := r :: !requests ;
      Lwt.return_unit
  end
  : Dummy_rpc )

let tests =
  [ ( "should be able to wrap a request with API definition"
      >:- fun _ ->
        let module A = struct
          type json = < > Js.t

          type params = int list

          type result = int

          let name = "sum"

          let params_to_json v = Array.of_list v |> Js.array |> Js.Unsafe.coerce

          let result_of_json = function _ -> 100
        end in
        let module R = (val dummy_rpc ()) in
        let module C = Jo.Client.Make (R) in
        let%lwt _ = C.call ~api:(module A) ~params:[1; 2; 3] () in
        let req = List.hd !R.requests in
        let expected =
          Jo.Request.
            { id= req.Jo.Request.id
            ; params= Some Js.(Unsafe.coerce @@ Js.array [|1; 2; 3|])
            ; _method= "sum" }
        in
        assert_ok (expected = req) |> Lwt.return )
  ; ( "should be able to wrap a notification with API definition"
      >:- fun _ ->
        let module B = struct
          type json = < > Js.t

          type params = int list

          type result = int

          let name = "sum"

          let params_to_json v = Js.Unsafe.coerce @@ Js.array @@ Array.of_list v

          let result_of_json = function _ -> 1
        end in
        let module R = (val dummy_rpc ()) in
        let module C = Jo.Client.Make (R) in
        let%lwt () = C.notify ~api:(module B) ~params:[1; 2; 3] () in
        let req = List.hd !R.requests in
        let expected =
          Jo.Request.
            { id= None
            ; params= Some Js.(Unsafe.coerce @@ Js.array [|1; 2; 3|])
            ; _method= "sum" }
        in
        assert_ok (expected = req) |> Lwt.return ) ]
