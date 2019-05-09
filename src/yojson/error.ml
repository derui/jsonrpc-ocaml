module J = Jsonrpc_ocaml
module B = Yojson.Safe

(** The type of error object *)
type json = Yojson.Safe.t

type t =
  { code : J.Types.Error_code.t
  ; data : json option }

let to_json error =
  let module T = Jsonrpc_ocaml.Types in
  let code = ("code", `Int (T.Error_code.to_int error.code)) in
  let message = ("message", `String (T.Error_code.to_message error.code)) in
  let data = match error.data with Some data -> [("data", data)] | None -> [] in
  `Assoc ([code; message] @ data)

let of_json = function
  | `Assoc assoc ->
      let code = List.assoc "code" assoc |> B.Util.to_int
      and message = List.assoc "message" assoc |> B.Util.to_string
      and data = List.assoc_opt "data" assoc in
      let module T = Jsonrpc_ocaml.Types in
      let code = T.Error_code.make ~message code in
      Ok {code; data}
  | _ as js -> Error (J.Types.Parse_error.Invalid_object js)
