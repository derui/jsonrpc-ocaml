module Js = Js_of_ocaml.Js

include
  Jsonrpc_ocaml.Response.S with type json = < > Js.t with module Error = Error
