
module type S = sig
  (** Abstract type for json *)
  type json

  module Error: Error_intf.S with type json = json

  (** The module handle response object *)
  type t = {
    result : json option;
    id : Types.id option;
    error : Error.t option;
  }

  val to_json : t -> json
  val of_json : json -> (t, json Types.Parse_error.t) result

end
