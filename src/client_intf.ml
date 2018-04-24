open Yojson.Basic

(** The signature to define interface of an API *)
module type Api_def = sig
  type params
  type result

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

  val call_api: ?handler:(Response.t -> unit) -> Request.t -> unit Thread.t
end
