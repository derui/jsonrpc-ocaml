module J = Jsonrpc_ocaml
module B = Yojson.Safe

(** The type of error object *)
type json = Yojson.Safe.t

include J.Error.Make_base (struct
  type t = json
end)

let to_json error =
  let module T = Jsonrpc_ocaml.Types in
  let code = ("code", `Int error.code) in
  let message = ("message", `String error.message) in
  let data = match error.data with Some data -> [("data", data)] | None -> [] in
  `Assoc ([code; message] @ data)

let of_json = function
  | `Assoc assoc ->
      let code = List.assoc "code" assoc |> B.Util.to_int
      and message = List.assoc "message" assoc |> B.Util.to_string
      and data = List.assoc_opt "data" assoc in
      let code = J.Types.Error_code.make ~message code in
      Ok (make ?data code)
  | _ -> Error J.Types.Parse_error.Invalid_object
