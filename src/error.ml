open Types

(** The type of error object *)
module type S = sig
  type json

  type t =
    { code : Error_code.t
    ; data : json option }

  val to_json : t -> json
  val of_json : json -> (t, json Parse_error.t) result
end
