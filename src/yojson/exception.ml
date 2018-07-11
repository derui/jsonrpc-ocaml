open Jsonrpc_ocaml

include Exception.Make(struct
    type t = Yojson.Safe.json
  end)
