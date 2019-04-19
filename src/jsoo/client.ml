module J = Jsonrpc_ocaml

module type Api_def = sig
  include J.Client.Api_def with type json = < > Js_of_ocaml.Js.t
end

module type S = J.Client.S with type json := < > Js_of_ocaml.Js.t

module Make (R : J.Rpc.S with type json = < > Js_of_ocaml.Js.t) :
  S with module Thread := R.Thread =
  J.Client.Make (struct
      type t = < > Js_of_ocaml.Js.t
    end)
    (R)
