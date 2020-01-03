module J = Jsonrpc
open J.Types

module type Data = sig
  type t
end

(** Convert exception to jsonrpc-defined error response *)
let handle_error request error =
  Response.{ result = None; id = request.Request.id; error = Some error }

module Make
    (D : Data)
    (Ser : Jsonrpc.Serializer.S
             with type json := Yojson.Safe.t
              and type data := D.t
             with module Request := Request)
    (De : Jsonrpc.Deserializer.S
            with type json := Yojson.Safe.t
             and type data := D.t
            with module Response := Response) =
struct
  type data = D.t
  type json = Yojson.Safe.t

  module Response = Response
  module Request = Request
  module Error = Error
  module Thread = Lwt

  type handler = json option -> (json option, Error.t) result Lwt.t
  type _method = string
  type t = { procedure_table : (_method, handler) Hashtbl.t }

  let make () = { procedure_table = Hashtbl.create 0 }

  let expose ~_method ~handler t =
    Hashtbl.add t.procedure_table _method handler;
    t

  let unexpose ~_method t =
    if Hashtbl.mem t.procedure_table _method then Hashtbl.remove t.procedure_table _method else ();
    t

  let handle_request ~(request : data) t =
    let open Lwt in
    let%lwt response =
      match Ser.serialize request with
      | Error _ ->
          let error = Error.(make Error_code.Parse_error) in
          Lwt.return Response.{ result = None; id = None; error = Some error }
      | Ok request ->
          Lwt.catch
            (fun () ->
              match Hashtbl.find_opt t.procedure_table request.Request._method with
              | None -> raise Error.(Jsonrpc_error (make Error_code.Method_not_found))
              | Some handler -> (
                  match%lwt handler request.params with
                  | Ok result -> return { Response.empty with id = request.id; result }
                  | Error e -> return { Response.empty with id = request.id; error = Some e } ))
            (function
              | Error.Jsonrpc_error error -> Lwt.return @@ handle_error request error
              | _ as e -> raise e)
    in
    return @@ De.deserialize response
end
