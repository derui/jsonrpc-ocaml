open Jsonrpc_ocaml.Types

type json = Yojson.Safe.json

(** The module for request object *)
type t = {_method: string; params: json option; id: id option}

let jsonrpc_version = ("jsonrpc", `String "2.0")

let is_request assoc =
  Predicates.(
    has_version assoc
    && has_id assoc
    && has_method assoc
  )

let is_notification assoc =
  Predicates.(
    has_version assoc
    && not @@ has_id assoc
    && has_method assoc
  )

(* conversion function between json and OCaml *)
let to_json t =
  let id' = match t.id with
    | Some id -> [("id", `String (Int64.to_string id))]
    | None -> []
  in
  let _method' = ("method", `String t._method) in
  let param' = match t.params with
    | Some params -> [("params", params)]
    | None -> []
  in

  `Assoc ([jsonrpc_version; _method';] @ param' @ id')

let of_json js =
  match js with
  | `Assoc assoc -> begin
      let open Jsonrpc_ocaml.Types.Parse_error in
      if is_notification assoc || is_request assoc then
        let id = List.assoc_opt "id" assoc
        and _method = List.assoc_opt "method" assoc
        and params = List.assoc_opt "params" assoc in
        let module U = Yojson.Safe.Util in
        match (id, _method) with
        | (None, Some m) -> Ok ({_method = U.to_string m;params; id = None})
        | (Some id, Some m) -> Ok ({id = Some (Int64.of_string @@ U.to_string id);
                                    _method = U.to_string m;
                                    params})
        | _ -> Error Invalid_request
      else
        Error Invalid_request
    end
  | _ -> Error (Invalid_object js)

(**/**)
(* ignore ocamldoc below *)

module Test = struct
  open OUnit

  let tests = [
    "should be able to encode request object to json", (fun () ->
        let expected_json = {|
{"jsonrpc": "2.0", "params": [1,2,3], "method": "sum", "id": "2"}
|}
        in
        let expected_json = Yojson.Safe.from_string expected_json in
        let request = {id = Some 2L; _method = "sum"; params = Some (`List [`Int 1;`Int 2;`Int 3]) } in
        (* sort key to compare with (=)  *)
        let actual = Yojson.Safe.sort @@ to_json request
        and expected = Yojson.Safe.sort expected_json in
        assert_equal actual expected
      );

    "should be able to encode a request object for notification to json", (fun () ->
        let expected_json = {|
{"jsonrpc": "2.0", "params": [1,2,3], "method": "sum"}
|}
        in
        let expected_json = Yojson.Safe.from_string expected_json in
        let request = {id = None; _method = "sum"; params = Some (`List [`Int 1;`Int 2;`Int 3]) } in
        (* sort key to compare with (=)  *)
        let actual = Yojson.Safe.sort @@ to_json request
        and expected = Yojson.Safe.sort expected_json in
        assert_equal actual expected
      );
    "should be able to handle object that omits params", (fun () ->
        let expected_json = {|
{"jsonrpc": "2.0", "method": "sum", "id": "2"}
|}
        in
        let expected_json = Yojson.Safe.from_string expected_json in
        let request = {id = Some 2L; _method = "sum"; params = None } in
        (* sort key to compare with (=)  *)
        let actual = Yojson.Safe.sort @@ to_json request
        and expected = Yojson.Safe.sort expected_json in
        assert_equal actual expected
      );
  ]

end
