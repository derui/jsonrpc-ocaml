module J = Jsonrpc_ocaml
open J.Types

(** Convert exception to jsonrpc-defined error response *)
let handle_error request = function
  | Types.Error_code.Internal_error
  | Invalid_params
  | Method_not_found
  | Invalid_request
  | Parse_error
  | Others _ as code ->
    let message = Types.Error_code.to_message code in
    Response.{
      result = None; id = request.Request.id;
      error = Some Error.{message;code;data = None}
    }

module Core = struct
  type json = Yojson.Safe.json
  module Response = Response
  module Request = Request
  module Thread = Lwt
  type handler = Request.t -> Response.t Lwt.t
  type _method = string
  type t = {
    procedure_table: (_method, handler) Hashtbl.t
  }

  let make () = {
    procedure_table = Hashtbl.create 0;
  }

  let expose ~_method ~handler t =
    Hashtbl.add t.procedure_table _method handler;
    t

  let unexpose ~_method t =
    if Hashtbl.mem t.procedure_table _method then
      Hashtbl.remove t.procedure_table _method
    else
      ();
    t

  let handle_request ~request t =
    match Hashtbl.find_opt t.procedure_table request.Request._method with
    | None -> Lwt.return @@ handle_error request Error_code.Method_not_found
    | Some handler -> Lwt.catch
                        (fun () -> handler request)
                        (function | Jsonrpc_error code -> Lwt.return @@ handle_error request code
                                  | _ as e -> raise e)
end

include Core
