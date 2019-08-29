open Types

module type S = sig
  type json
  (** Abstract type for json *)

  type t =
    { _method : string
    ; params : json option
    ; id : id option }
  (** The module handle request object *)

  val to_json : t -> json
  val of_json : json -> (t, Types.Parse_error.t) result
end
