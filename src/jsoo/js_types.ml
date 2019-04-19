module Js = Js_of_ocaml.Js

class type jsonrpc_version =
  object
    method jsonrpc : Js.js_string Js.t Js.readonly_prop
  end

class type error =
  object
    method code : int Js.optdef_prop

    method message : Js.js_string Js.t Js.optdef_prop

    method data : < > Js.t Js.optdef_prop
  end

class type response =
  object
    inherit jsonrpc_version

    method result : < > Js.t Js.optdef_prop

    method id : Js.js_string Js.t Js.optdef_prop

    method error : error Js.t Js.optdef_prop
  end

class type request =
  object
    inherit jsonrpc_version

    method _method : Js.js_string Js.t Js.optdef_prop

    method params : < > Js.t Js.optdef_prop

    method id : Js.js_string Js.t Js.optdef_prop
  end
