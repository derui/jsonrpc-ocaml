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

(** A module signature for Threading such as Lwt. *)
module type Thread = sig
  type 'a t

  val bind: 'a t -> ('a -> 'b t) -> 'b t
  val return: 'a -> 'a t
end

(** A module signature to call JSON-RPC. User can create RPC caller with this
    module.
*)
module type Rpc = sig
  module Thread: Thread
  module Response: Response_intf.S
  module Request: Request_intf.S

  val call_api: ?handler:(Response.t -> unit) -> Request.t -> unit Thread.t
end

(** A module signature to create request for JSON-RPC. Using this module combines with a module implemented
    Rpc module interface. *)
module type Client = sig
  type json
  module Response: Response_intf.S with type json = json
  module Request: Request_intf.S with type json = json

  (** Make a request for API *)
  val make_request:
    (module Api_def with type params = 'p and type result = 'r and type json = json)
    -> 'p option
    -> (('r, Response.Error.t) result -> unit)
    -> Request.t * (('r, Response.Error.t) result option)

  (** Make a notification for API *)
  val make_notification:
    (module Api_def with type params = 'p and type result = 'r)
    -> 'p option
    -> Request.t * (('r, Response.Error.t) result option)
end
