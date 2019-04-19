open Jsonrpc_ocaml
module Js = Js_of_ocaml.Js

include Exception.Make (struct
  type t = < > Js.t
end)
