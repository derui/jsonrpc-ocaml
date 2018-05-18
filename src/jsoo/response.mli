
type json = < > Js.t

module Error = Error

type t = {
  result : json option;
  id : Jsonrpc_ocaml.Types.id option;
  error : Error.t option;
}

val to_json : t -> json
val of_json : json -> (t, json Jsonrpc_ocaml.Types.Parse_error.t) result

module Test : sig
  val tests : (string * (unit -> Mocha_of_ocaml.assertion)) list
end
