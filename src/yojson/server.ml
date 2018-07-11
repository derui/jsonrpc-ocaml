module J = Jsonrpc_ocaml
open J.Types

module E = J.Exception.Make(struct
    type t = Yojson.Safe.json
  end)

(** Convert exception to jsonrpc-defined error response *)
let handle_error request data code =
  Response.{
    result = None; id = request.Request.id;
    error = Some Error.{code;data = data}
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
    Lwt.catch (fun () ->
        match Hashtbl.find_opt t.procedure_table request.Request._method with
        | None -> E.raise_error Error_code.Method_not_found
        | Some handler -> handler request
      )
      (function | E.Jsonrpc_error (code, data) -> Lwt.return @@ handle_error request data code
                | _ as e -> raise e)
end

include Core
