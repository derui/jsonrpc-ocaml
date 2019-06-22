(** Define response module for Yojson *)

include Jsonrpc.Response.S with type json = Yojson.Safe.t with type error = Error.t
