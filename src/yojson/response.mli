include
  Jsonrpc_ocaml.Response.S
  with type json = Yojson.Safe.json
  with module Error = Error
