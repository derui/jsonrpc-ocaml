module J = Jsonrpc_ocaml

module type Api_def = sig
  include J.Client.Api_def with type json = < > Js.t
end

module type S = J.Client.S with type json := < > Js.t

module Make (R : J.Rpc.S with type json = < > Js.t) :
  S with module Thread := R.Thread =
  J.Client.Make (struct
    type t = < > Js.t
  end)
    (R)
