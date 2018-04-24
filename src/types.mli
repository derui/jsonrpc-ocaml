
(** The type of id that is used in request *)
type id = int64

(** Algebric types for error code *)
module Error_code : sig
  type t =
      Parse_error
    | Invalid_request
    | Method_not_found
    | Invalid_params
    | Internal_error
    | Others of int

  val of_int : int -> t
  val to_int : t -> int
end

(** The module handle error object *)
module Error : sig
  type t = {
    code : Error_code.t;
    message : string;
    data : Yojson.Basic.json option;
  }
  val to_json : t -> [> `Assoc of (string * Yojson.Basic.json) list ]
  val of_json : Yojson.Basic.json -> (t, Errors.t) result
end

type 'a response_handler = ('a, Error.t) result -> unit

(** The module handle request object *)
module Request : sig
  type t = {
    _method : string;
    params : Yojson.Basic.json option;
    id : id option;
  }
  val to_json : t -> Yojson.Basic.json
  val of_json : Yojson.Basic.json -> (t, Errors.t) result
end

(** The module handle response object *)
module Response : sig
  type t = {
    result : Yojson.Basic.json option;
    id : id option;
    error : Error.t option;
  }
  (** Predicates to detect either success or error a response  *)
  val is_error : t -> bool
  val is_success : t -> bool

  val to_json : t -> Yojson.Basic.json
  val of_json : Yojson.Basic.json -> (t, Errors.t) result
end

module Test : sig
  val tests : (string * (unit -> bool)) list
end
