module J = Jsonrpc
open J.Types

(** Convert exception to jsonrpc-defined error response *)
let handle_error request data code =
  Response.{result = None; id = request.Request.id; error = Some Error.(make ?data code)}

module Core = struct
  type json = Yojson.Safe.t

  module Response = Response
  module Request = Request
  module Error = Error
  module Thread = Lwt

  type handler = json option -> (json option, Error.t) result Lwt.t
  type _method = string
  type t = {procedure_table : (_method, handler) Hashtbl.t}

  let make () = {procedure_table = Hashtbl.create 0}

  let expose ~_method ~handler t =
    Hashtbl.add t.procedure_table _method handler ;
    t

  let unexpose ~_method t =
    if Hashtbl.mem t.procedure_table _method then Hashtbl.remove t.procedure_table _method else () ;
    t

  let handle_request ~request t =
    Lwt.catch
      (fun () ->
        match Hashtbl.find_opt t.procedure_table request.Request._method with
        | None -> Exception.raise_error Error_code.Method_not_found
        | Some handler -> (
            let open Lwt in
            handler request.params
            >>= function
            | Ok result -> return {Response.empty with id = request.id; result}
            | Error e -> return {Response.empty with id = request.id; error = Some e} ) )
      (function
        | Exception.Jsonrpc_error (code, data) -> Lwt.return @@ handle_error request data code
        | _ as e -> raise e )
end

include Core
