module J = Jsonrpc_ocaml
module Js = Js_of_ocaml.Js

(** The type of error object *)
type json = < > Js.t

type t = {code: J.Types.Error_code.t; data: json option}

class type js =
  object
    method code : int Js.readonly_prop

    method message : Js.js_string Js.t Js.readonly_prop

    method data : json Js.optdef Js.readonly_prop
  end

let to_json error =
  let module T = Jsonrpc_ocaml.Types in
  let code = ("code", Js.Unsafe.inject (T.Error_code.to_int error.code)) in
  let message =
    ( "message"
    , Js.Unsafe.inject @@ Js.string (T.Error_code.to_message error.code) )
  in
  let data =
    match error.data with
    | Some data ->
        [|("data", Js.Unsafe.inject data)|]
    | None ->
        [||]
  in
  Js.Unsafe.obj @@ Array.concat [[|code; message|]; data]

let of_json js =
  let js : js Js.t = Js.Unsafe.coerce js in
  let module T = Jsonrpc_ocaml.Types in
  let code = T.Error_code.make ~message:Js.(to_string js##.message) js##.code
  and data = Js.Optdef.to_option js##.data in
  Ok {code; data}
