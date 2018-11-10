(** This module provides functions to handle error of JSON-RPC. *)

module type Type = sig
  type t
end

(** Interface for exception with detail data of error. *)
module type S = sig
  type json

  (** Common exception of JSON-RPC. All exceptions presents with error code and
      detail object if it have.
      Do not raise this exception directly, should use {!raise_error} when you want to raise error.
  *)
  exception Jsonrpc_error of Types.Error_code.t * json option

  val raise_error : ?data:json -> Types.Error_code.t -> 'a
  (** [raise_error ?data error_code] raises exception with code and data.

      @raise Jsonrpc_error always raises.
  *)
end

(** Make the module for exception. *)
module Make (T : Type) : S with type json = T.t = struct
  type json = T.t

  exception Jsonrpc_error of Types.Error_code.t * json option

  let raise_error ?data code = raise (Jsonrpc_error (code, data))
end
