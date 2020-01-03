(** Define response module for Yojson *)

module type Data = sig
  type t
end

module Make
    (D : Data)
    (Ser : Jsonrpc.Serializer.S
             with type json := Yojson.Safe.t
              and type data := D.t
             with module Request := Request)
    (De : Jsonrpc.Deserializer.S
            with type json := Yojson.Safe.t
             and type data := D.t
            with module Response := Response) :
  Jsonrpc.Server.S
    with type json = Yojson.Safe.t
     and type data = D.t
    with module Request = Request
     and module Response = Response
     and module Error = Error
     and module Thread = Lwt
