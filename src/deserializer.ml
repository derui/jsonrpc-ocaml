(** The deserializer module that has function to deserialize a OCaml response to raw data. *)

module type S = sig
  type data
  (** data to deserialize *)

  type json
  (** json type for response *)

  module Response : Response.S with type json = json

  val deserialize : Response.t -> data
  (** [deserialize response] deserialize a response object to data *)
end
