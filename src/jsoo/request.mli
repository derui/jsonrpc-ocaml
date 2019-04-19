module Js = Js_of_ocaml.Js
open Jsonrpc_ocaml

include Request.S with type json = < > Js.t
