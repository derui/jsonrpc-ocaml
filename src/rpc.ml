(** A module signature for Threading such as Lwt. *)
module type Thread = sig
  type 'a t

  val bind : 'a t -> ('a -> 'b t) -> 'b t

  val return : 'a -> 'a t
end

(** A module signature to call JSON-RPC. User can create RPC caller with this
    module.
*)
module type S = sig
  type json

  module Thread : Thread

  module Response : Response.S with type json := json

  module Request : Request.S with type json := json

  val call : Request.t -> Response.t Thread.t
  (** [call request] calls remote method via [request]. Notice thread that is returned from this
      continues after received response or error from server.
  *)

  val notify : Request.t -> unit Thread.t
  (** [notify request] send notification to server with [request]. Notice thread that is returned from this
      continues after finished to send request.
  *)
end
