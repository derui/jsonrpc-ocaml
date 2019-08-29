open Types

module type Base = sig
  type data

  type t = private
    { code : int
    ; message : string
    ; data : data option }

  exception Jsonrpc_error of t

  val to_error_code : t -> Error_code.t
  val make : ?data:data -> Error_code.t -> t
  val to_string : t -> string
end

module type S = sig
  type json

  include Base with type data := json

  val to_json : t -> json
  val of_json : json -> (t, Parse_error.t) result
end

module type Type = sig
  type t
end

module Make_base (T : Type) : Base with type data := T.t = struct
  type t =
    { code : int
    ; message : string
    ; data : T.t option }

  exception Jsonrpc_error of t

  let to_error_code {code; message; _} = Error_code.make ~message code

  let make ?data code =
    let code' = Error_code.to_int code in
    let message = Error_code.to_string code in
    {code = code'; message; data}

  let to_string {code; message; _} = Printf.sprintf "Jsonrpc_error(%d, %s)" code message
end
