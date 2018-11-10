module J = Jsonrpc_ocaml

module type Api_def = sig
  include J.Client.Api_def with type json = Yojson.Safe.json
end

module type S = J.Client.S with type json = Yojson.Safe.json

module Make (R : J.Rpc.S with type json = Yojson.Safe.json) =
  J.Client.Make (struct
      type t = Yojson.Safe.json
    end)
    (R)
