module Js = Js_of_ocaml.Js
open Jsonrpc_ocaml.Types

type json = < > Js.t

module Error = Error

let jsonrpc_version = ("jsonrpc", Js.Unsafe.inject @@ Js.string "2.0")

type t = {result: json option; id: id option; error: Error.t option}

let empty = {result= None; id= None; error= None}

let is_response_success assoc =
  Predicates.(
    has_version assoc && has_result assoc && has_id assoc
    && (not @@ has_error assoc))

let is_response_error assoc =
  Predicates.(
    has_version assoc
    && (not @@ has_result assoc)
    && has_nullable_id assoc && has_error assoc)

let to_json t =
  let id =
    match t.id with
    | None ->
        []
    | Some id ->
        [("id", Js.Unsafe.inject @@ Js.string (Int64.to_string id))]
  in
  let error =
    match t.error with
    | None ->
        []
    | Some error ->
        [("error", Js.Unsafe.inject @@ Error.to_json error)]
  in
  let result =
    match t.result with
    | None ->
        []
    | Some result ->
        [("result", Js.Unsafe.inject result)]
  in
  Js.Unsafe.obj (Array.of_list ([jsonrpc_version] @ id @ error @ result))

(** Predicate to detect a object is error or not *)
let of_json js =
  let js : Js_types.response Js.t = Js.Unsafe.coerce js in
  if is_response_success js || is_response_error js then
    let id = Js.Optdef.map js##.id Js.to_string |> Js.Optdef.to_option
    and error = Js.Optdef.to_option js##.error
    and result = Js.Optdef.to_option js##.result in
    let id =
      match id with Some id -> Some (Int64.of_string id) | None -> None
    in
    match error with
    | Some error -> (
      match Error.of_json error with
      | Ok error ->
          Ok {error= Some error; id; result}
      | Error _ as e ->
          e )
    | None ->
        Ok {error= None; id; result}
  else Error Jsonrpc_ocaml.Types.Parse_error.Invalid_response
