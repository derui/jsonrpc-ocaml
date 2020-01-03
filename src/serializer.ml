(** The serializer module that has serialize a request to OCaml object. *)

module type S = sig
  type data
  (** data to serialize *)

  type json
  (** json type for request *)

  module Request : Request.S with type json = json

  val serialize : data -> (Request.t, string) result
  (** [serialize data] serialize [data] to a request object *)
end
