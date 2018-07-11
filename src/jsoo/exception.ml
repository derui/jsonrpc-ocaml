open Jsonrpc_ocaml

include Exception.Make(struct
    type t = < > Js.t
  end)
