open Types
open Yojson.Basic

(** The signature of method *)
module type Method = sig
  type param
  type result

  (** Return name of method *)
  val name: string

  (** convert parameter to json *)
  val param_to_json: param option -> json option

  (** convert json to result when request success. *)
  val result_of_json: json -> result
end

module type RPC = sig
  type param
  type result

  val request_rpc: ?id:[`Use_random_gen | `Use_specific of int64] -> param option -> (int64 * jsonrpc)

  val response_of_rpc: id:int64 -> response:jsonrpc -> (result, error) Pervasives.result
end
