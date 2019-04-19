module Js = Js_of_ocaml.Js

(** Specialized module interface with Js_of_ocaml  *)
module type S =
  Jsonrpc_ocaml.Rpc.S
  with type json = < > Js.t
   and module Response = Response
   and module Request = Request
