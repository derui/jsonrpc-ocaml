module type S = sig
  (** Abstract type for json *)
  type json

  module Error : Error.S with type json = json

  (** The module handle response object *)
  type t =
    { result : json option
    ; id : Types.id option
    ; error : Error.t option }

  val empty : t
  (** [empty] returns empty response that all element are none of.  *)

  val to_json : t -> json
  val of_json : json -> (t, Types.Parse_error.t) result
end
