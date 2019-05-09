module J = Jsonrpc_ocaml

module type Api_def = sig
  include J.Client.Api_def with type json = Yojson.Safe.t
end

module type S = J.Client.S with type json := Yojson.Safe.t

module Make (R : J.Client.Raw with type json = Yojson.Safe.t) : S with module Thread := R.Thread =
  J.Client.Make (struct
      type t = Yojson.Safe.t
    end)
    (R)
