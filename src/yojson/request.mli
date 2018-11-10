open Jsonrpc_ocaml

include Request.S with type json = Yojson.Safe.json
