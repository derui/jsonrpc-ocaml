
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
