open Jsonrpc_ocaml.Types

type json = < > Js.t

(** The module for request object *)
type t = {_method: string; params: json option; id: id option}

let jsonrpc_version = ("jsonrpc", Js.(Unsafe.inject @@ string "2.0"))

let is_request assoc =
  Predicates.(
    has_version assoc
    && has_id assoc
    && has_method assoc
    && has_params assoc
  )

let is_notification assoc =
  Predicates.(
    has_version assoc
    && not @@ has_id assoc
    && has_method assoc
    && has_params assoc
  )

(* conversion function between json and OCaml *)
let to_json t =
  let id' = match t.id with
    | Some id -> [("id", Js.(Unsafe.inject @@ string (Int64.to_string id)))]
    | None -> []
  in
  let _method' = ("method", Js.(Unsafe.inject @@ string t._method)) in
  let param' = match t.params with
    | Some params -> [("params", Js.Unsafe.inject params)]
    | None -> []
  in

  Js.Unsafe.obj (Array.of_list ([jsonrpc_version;_method'] @ id' @ param'))

let of_json js =
  let js : Js_types.request Js.t = Js.Unsafe.coerce js in
  let open Jsonrpc_ocaml.Types.Parse_error in
  if is_notification js || is_request js then
    let id = Js.Optdef.to_option js##.id
    and _method = Js.Optdef.to_option js##._method
    and params = Js.Optdef.to_option js##.params in
    match (id, _method) with
    | (None, Some m) -> Ok ({_method = Js.to_string m;params; id = None})
    | (Some id, Some m) -> Ok ({id = Some (Int64.of_string @@ Js.to_string id);
                                _method = Js.to_string m;
                                params})
    | _ -> Error Invalid_request
  else
    Error Invalid_request

(**/**)
(* ignore ocamldoc below *)

module Test = struct

  open Mocha_of_ocaml
  let tests = [
    "should be able to encode request object to json", (fun () ->
        let expected_json = {|
{"jsonrpc": "2.0", "params": [1,2,3], "method": "sum", "id": "2"}
|}
        in
        let expected = Js._JSON##parse (Js.string expected_json) in
        let request = {id = Some 2L;
                       _method = "sum";
                       params = Some (Js.Unsafe.coerce @@ Js.(array [|1;2;3|]))
                      } in
        (* sort key to compare with (=)  *)
        let actual = to_json request in
        assert_strict_eq actual expected
      );

    "should be able to encode a request object for notification to json", (fun () ->
        let expected_json = {|
{"jsonrpc": "2.0", "params": [1,2,3], "method": "sum"}
|}
        in
        let expected = Js._JSON##parse (Js.string expected_json) in
        let request = {id = None;
                       _method = "sum";
                       params = Some (Js.Unsafe.coerce @@ Js.(array [|1;2;3|]))
                      } in
        (* sort key to compare with (=)  *)
        let actual = to_json request in
        assert_strict_eq actual expected
      );
  ]

end
