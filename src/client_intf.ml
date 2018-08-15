(** The signature to define interface of an API *)
module type Api_def = sig
  type params
  type result
  type json

  (** Return name of method *)
  val name: string

  (** convert parameter to json *)
  val params_to_json: params option -> json option

  (** convert json to result when request success. *)
  val result_of_json: json -> result
end

(** A module signature to create request for JSON-RPC. Using this module combines with a module implemented
    Rpc module interface. *)
module type S = sig
  type json
  module Response: Response_intf.S with type json = json
  module Request: Request_intf.S with type json = json

  (** Make a request for API *)
  val make_request:
    (module Api_def with type params = 'p and type result = 'r and type json = json)
    -> 'p option
    -> (('r option, Response.Error.t) result -> unit)
    -> Request.t * (('r option, Response.Error.t) result option)

  (** Make a notification for API *)
  val make_notification:
    (module Api_def with type params = 'p and type result = 'r)
    -> 'p option
    -> Request.t * (('r option, Response.Error.t) result option)
end
