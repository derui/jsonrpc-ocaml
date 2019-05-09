(** Defines error module for Yojson *)

open Jsonrpc_ocaml
include Error.S with type json = Yojson.Safe.t
