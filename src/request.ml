open Types
open Errors
open Yojson.Basic

(** The module for request object *)
type t = {_method: string; params: json option; id: id option}

let jsonrpc_version = ("jsonrpc", `String "2.0")

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
      if is_notification assoc || is_request assoc then
        let id = List.assoc_opt "id" assoc
        and _method = List.assoc_opt "method" assoc
        and params = List.assoc_opt "params" assoc in
        match (id, _method) with
        | (None, Some m) -> Ok ({_method = Util.to_string m;params; id = None})
        | (Some id, Some m) -> Ok ({id = Some (Int64.of_string @@ Util.to_string id);
                                    _method = Util.to_string m;
                                    params})
        | _ -> Error Invalid_request
      else
        Error Invalid_request
    end
  | _ -> Error (Invalid_object js)

(**/**)
(* ignore ocamldoc below *)

module Test = struct

  let tests = [
    "should be able to encode request object to json", (fun () ->
        let expected_json = {|
{"jsonrpc": "2.0", "params": [1,2,3], "method": "sum", "id": "2"}
|}
        in
        let expected_json = from_string expected_json in
        let request = {id = Some 2L; _method = "sum"; params = Some (`List [`Int 1;`Int 2;`Int 3]) } in
        (* sort key to compare with (=)  *)
        let actual = Yojson.Basic.sort @@ to_json request
        and expected = Yojson.Basic.sort expected_json in
        actual = expected
      );

    "should be able to encode a request object for notification to json", (fun () ->
        let expected_json = {|
{"jsonrpc": "2.0", "params": [1,2,3], "method": "sum"}
|}
        in
        let expected_json = from_string expected_json in
        let request = {id = None; _method = "sum"; params = Some (`List [`Int 1;`Int 2;`Int 3]) } in
        (* sort key to compare with (=)  *)
        let actual = Yojson.Basic.sort @@ to_json request
        and expected = Yojson.Basic.sort expected_json in
        actual = expected
      );
  ]

end
