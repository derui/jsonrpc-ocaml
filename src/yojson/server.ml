module J = Jsonrpc_ocaml
open J.Types

module Core = struct
  type json = Yojson.Basic.json
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
    | None -> raise (Jsonrpc_error Error_code.Method_not_found)
    | Some handler -> handler request

end

include Core
