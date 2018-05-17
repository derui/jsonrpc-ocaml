open Jsonrpc_ocaml.Types

type json = Yojson.Basic.json

module Error = Error

let jsonrpc_version = ("jsonrpc", `String "2.0")

type t = {
  result: json option;
  id: id option;
  error: Error.t option;
}

let is_response_success assoc =
  Predicates.(
    has_version assoc
    && has_result assoc
    && has_id assoc
    && not @@ has_error assoc
  )

let is_response_error assoc =
  Predicates.(
    has_version assoc
    && not @@ has_result assoc
    && has_nullable_id assoc
    && has_error assoc
  )

let to_json t =
  let id = match t.id with
    | None -> []
    | Some id -> [("id", `String (Int64.to_string id))]
  in
  let error = match t.error with
    | None -> []
    | Some error -> [("error", Error.to_json error)]
  in
  let result = match t.result with
    | None -> []
    | Some result -> [("result", result)]
  in

  `Assoc ([jsonrpc_version] @ id @ error @ result)

(** Predicate to detect a object is error or not *)
let is_error t = match (t.result, t.error) with
  | None, Some _ -> true
  | _, _ -> false

let is_success t = match (t.result, t.error) with
  | Some _, None -> true
  | _, _ -> false

let of_json  js =
  match js with
  | `Assoc assoc -> begin
      if is_response_success assoc || is_response_error assoc then
        let id = List.assoc_opt "id" assoc
        and error = List.assoc_opt "error" assoc
        and result = List.assoc_opt "result" assoc in
        let id = match id with
          | Some id -> Some (Int64.of_string @@ Yojson.Basic.Util.to_string id)
          | None -> None
        in
        match error with
        | Some error -> begin
            match (Error.of_json error) with
            | Ok error -> Ok ({error = Some error;id;result})
            | Error _ as e -> e
          end
        | None -> Ok ({error = None; id;result})
      else
        Error Invalid_response
    end
  | _ -> Error (Invalid_object js)

(**/**)
(* ignore ocamldoc below *)

module Test = struct

  let tests = [
    "should be able to parse response object from json", (fun () ->
        let json = {|
{"jsonrpc": "2.0", "result": 19, "id": "1"}
|}
        in
        let expected = {id = Some 1L; result = Some (`Int 19); error = None} in
        of_json (Yojson.Basic.from_string json) = Ok expected
      );

    "should be able to parse error response object from json", (fun () ->
        let json = {|
{"jsonrpc": "2.0", "error": {"code": -32700, "message": "error happenned"}, "id": "1"}
|}
        in
        let expected = {
            id = Some 1L;
            result = None;
            error = Some Error.{code = Error_code.Parse_error; message = "error happenned"; data = None}
          } in
        of_json (Yojson.Basic.from_string json) = Ok expected
      );

    "should be able to parse error response object with data from json", (fun () ->
        let json = {|
{"jsonrpc": "2.0", "error": {"code": -32700, "message": "error happenned", "data": "detail"}, "id": "1"}
|}
        in
        let expected = {
            id = Some 1L;
            error = Some Error.{code = Error_code.Parse_error; message = "error happenned"; data = Some (`String "detail")};
            result = None;
          } in
        of_json (Yojson.Basic.from_string json) = Ok expected
      );
  ]

end
