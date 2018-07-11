module J = Jsonrpc_ocaml

(** The type of error object *)
type json = < > Js.t
type t = {
  code: J.Types.Error_code.t;
  data: json option;
}

let to_json error =
  let module T = Jsonrpc_ocaml.Types in
  let code = ("code", Js.Unsafe.inject (T.Error_code.to_int error.code)) in
  let message = ("message", Js.Unsafe.inject @@ Js.string (T.Error_code.to_message error.code)) in
  let data = match error.data with
    | Some data -> [|("data", Js.Unsafe.inject data)|]
    | None -> [||]
  in
  Js.Unsafe.obj @@ Array.concat [[|code; message|];data]

let of_json js =
  let keys = Js.object_keys js |> Js.to_array |> Array.to_list in
  let obj = List.map (fun key -> (Js.to_string key, Js.Unsafe.get js key)) keys in
  let module T = Jsonrpc_ocaml.Types in
  let code = List.assoc "code" obj |> T.Error_code.of_int
  and data = List.assoc_opt "data" obj in

  Ok ({code;data;})
