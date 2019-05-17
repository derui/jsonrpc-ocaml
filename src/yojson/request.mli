(** Define request module for Yojson *)

open Jsonrpc
include Request.S with type json = Yojson.Safe.t
