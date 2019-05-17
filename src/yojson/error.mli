(** Defines error module for Yojson *)

open Jsonrpc
include Error.S with type json = Yojson.Safe.t
