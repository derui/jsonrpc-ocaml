open Jsonrpc_ocaml

include Request.S with type json = < > Js.t
