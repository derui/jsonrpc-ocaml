open Jsonrpc

include Exception.Make (struct
  type t = Yojson.Safe.t
end)
