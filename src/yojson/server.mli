(** Define response module for Yojson *)

include
  Jsonrpc.Server.S
  with type json = Yojson.Safe.t
  with module Request = Request
   and module Response = Response
   and module Error = Error
   and module Thread = Lwt
