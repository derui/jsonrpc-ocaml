(** A module signature to create procedure for JSON-RPC. Using this module combines with a module implemented
    Rpc module interface. *)
module type S = sig
  type t
  type json
  type data

  module Response : Response.S with type json = json
  module Request : Request.S with type json = json
  module Error : Error.S with type json = json
  module Thread : Types.Thread

  type handler = json option -> (json option, Error.t) result Thread.t
  type _method = string

  val make : unit -> t
  (** Make a server instance. *)

  val expose : _method:_method -> handler:handler -> t -> t
  (** Expose API with [handler] *)

  val unexpose : _method:string -> t -> t
  (** Unexpose API bound specified [_method] *)

  val handle_request : request:data -> t -> data Thread.t
  (** Handle [request] with server instance [t].
      This function will raise [jsonrpc_error] if can not handle request,
      not found method, or something else.
  *)
end
