(** This module provides functor to make client for specialized Thread module.  *)

module J = Jsonrpc

module Make (T : J.Types.Thread) =
  J.Client.Make_client (struct
      type t = Yojson.Safe.t
    end)
    (Error)
    (Request)
    (Response)
    (T)
