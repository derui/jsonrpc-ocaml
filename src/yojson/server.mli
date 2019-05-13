(** Define response module for Yojson *)

include
  Jsonrpc_ocaml.Server.S
  with type json = Yojson.Safe.t
  with module Request = Request
   and module Response = Response
   and module Thread = Lwt
