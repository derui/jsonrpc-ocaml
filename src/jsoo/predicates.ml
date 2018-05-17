open Js_types
let has_error_code js = not @@ Js.Optdef.test js##.code

let has_error_message js = not @@ Js.Optdef.test js##.message

let is_error : error Js.t -> bool = fun js -> has_error_code js && has_error_message js

let has_version js =
  let js : jsonrpc_version Js.t = Js.Unsafe.coerce js in
  js##.jsonrpc = Js.string "2.0"

let has_params js =
  let js : request Js.t = Js.Unsafe.coerce js in
  not @@ Js.Optdef.test js##.params

let has_id js =
  let js : request Js.t = Js.Unsafe.coerce js in
  not @@ Js.Optdef.test js##.id

let has_nullable_id _ = true

let has_method js =
  let js : request Js.t = Js.Unsafe.coerce js in
  not @@ Js.Optdef.test js##._method

let has_error js =
  let js : response Js.t = Js.Unsafe.coerce js in
  if not @@ Js.Optdef.test js##.error then
    let is_error = Js.Optdef.map js##.error is_error  |> Js.Optdef.to_option in
    is_error = Some true
  else
    false

let has_result js =
  let js : response Js.t = Js.Unsafe.coerce js in
  not @@ Js.Optdef.test js##.result
