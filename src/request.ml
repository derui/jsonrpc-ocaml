open Types

module type S = sig
  (** Abstract type for json *)
  type json

  (** The module handle request object *)
  type t = {_method: string; params: json option; id: id option}

  val to_json : t -> json

  val of_json : json -> (t, json Types.Parse_error.t) result
end
