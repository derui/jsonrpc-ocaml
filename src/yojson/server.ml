module J = Jsonrpc_ocaml
open J.Types

module Core = struct
  type json = Yojson.Basic.json
  module Response = Response
  module Request = Request
  type handler = Request.t -> Response.t option
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

(**/**)
(* ignore document below *)

module Test = struct
  open OUnit
  let tests = [
    ("should be able to expose method with handler", fun _ ->
        let server = Core.make () in
        let server = Core.expose server ~_method:"test"
            ~handler:(fun _ -> Some Response.{result = None;
                                         id = None;
                                         error = None})
        in
        let expected = Some Response.{result = None; id = None; error = None} in
        let actual = handle_request server ~request:Request.{
            _method = "test";
            params = None;
            id = None;
          }
        in
        assert_equal expected actual
    );
    ("should raise error if call method that does not expose", fun _ ->
        let server = Core.make () in
        let server = Core.expose server ~_method:"test"
            ~handler:(fun _ -> Some Response.{result = None;
                                              id = None;
                                              error = None})
        in
        assert_raises (Jsonrpc_error Error_code.Method_not_found) (fun _ ->
          handle_request server ~request:Request.{
            _method = "foo";
            params = None;
            id = None;
          })
    );
  ]
end
