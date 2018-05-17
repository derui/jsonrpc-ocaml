
type json = < > Js.t

(** The module handle request object *)
type t = {
  _method : string;
  params : json option;
  id : Jsonrpc_ocaml.Types.id option;
}
val to_json : t -> json
val of_json : json -> (t, json Jsonrpc_ocaml.Types.Parse_error.t) result

module Test : sig
  val tests : (string * (unit -> Mocha_of_ocaml.assertion)) list
end