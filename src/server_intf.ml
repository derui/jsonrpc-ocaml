(** A module signature for Threading such as Lwt. *)
module type Thread = sig
  type 'a t

  val bind: 'a t -> ('a -> 'b t) -> 'b t
  val return: 'a -> 'a t
end

(** A module signature to create procedure for JSON-RPC. Using this module combines with a module implemented
    Rpc module interface. *)
module type S = sig
  type t
  type json
  module Response: Response_intf.S with type json = json
  module Request: Request_intf.S with type json = json
  module Thread: Thread
  type handler = Request.t -> Response.t Thread.t
  type _method = string

  (** Make a server instance. *)
  val make: unit -> t

  (** Expose API with [handler] *)
  val expose:
    _method:_method
    -> handler:handler
    -> t -> t

  (** Unexpose API bound specified [_method] *)
  val unexpose: _method:string -> t -> t

  (** Handle [request] with server instance [t].
      This function will raise [jsonrpc_error] if can not handle request,
      not found method, or something else.
  *)
  val handle_request: request:Request.t -> t -> Response.t Thread.t
end