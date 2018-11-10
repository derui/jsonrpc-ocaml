include
  Jsonrpc_ocaml.Response.S with type json = < > Js.t with module Error = Error

module Test : sig
  val tests : (string * (unit -> Mocha_of_ocaml.assertion)) list
end
