
open Yojson.Basic
open Errors

type id = int64

(** Type type of error code defined by specification of JSON RPC *)
module Error_code = struct
  type t =
    | Parse_error
    | Invalid_request
    | Method_not_found
    | Invalid_params
    | Internal_error
    | Others of int

  let of_int = function
    | -32700 -> Parse_error
    | -32600 -> Invalid_request
    | -32601 -> Method_not_found
    | -32602 -> Invalid_params
    | -32603 -> Internal_error
    | _ as v -> Others v

  let to_int = function
    | Parse_error ->  -32700
    | Invalid_request ->  -32600
    | Method_not_found ->  -32601
    | Invalid_params ->  -32602
    | Internal_error ->  -32603
    | Others v -> v
end

(** The type of error object *)
module Error = struct

  type t = {
    code: Error_code.t;
    message: string;
    data: json option;
  }

  let to_json error =
    let code = ("code", `Int (Error_code.to_int error.code)) in
    let message = ("message", `String error.message) in
    let data = match error.data with
      | Some data -> [("data", data)]
      | None -> []
    in

    `Assoc ([code; message] @ data)

  let of_json = function
    | `Assoc assoc -> begin
        let code = List.assoc "code" assoc |> Util.to_int
        and message = List.assoc "message" assoc |> Util.to_string
        and data = List.assoc_opt "data" assoc in

        let code = Error_code.of_int code in
        Ok ({code;message; data})
      end
    | _ as js -> Error (Invalid_object js)
end

type 'a response_handler = ('a, Error.t) result -> unit

let jsonrpc_version = ("jsonrpc", `String "2.0")

module Predicates = struct

  let has_error_code assoc = match List.assoc_opt "code" assoc with
    | Some (`Int _) -> true
    | _ -> false

  let has_error_message assoc = match List.assoc_opt "message" assoc with
    | Some (`String _) -> true
    | _ -> false

  let is_error = function
    | `Assoc assoc -> begin
        has_error_code assoc && has_error_message assoc
      end
    | _ -> false

  let has_version assoc = List.assoc_opt "jsonrpc" assoc = Some (`String "2.0")
  let has_params assoc = match List.assoc_opt "params" assoc with
    | Some _ -> true
    | None -> false

  let has_id assoc = match List.assoc_opt "id" assoc with
    | Some (`String _) -> true
    | _ -> false

  let has_nullable_id assoc = match List.assoc_opt "id" assoc with
    | Some (`String _) | Some `Null -> true
    | _ -> false

  let has_method assoc = match List.assoc_opt "method" assoc with
    | Some (`String _) -> true
    | _ -> false

  let has_error assoc = match List.assoc_opt "error" assoc with
    | Some err -> is_error err
    | None -> false

  let has_result assoc = match List.assoc_opt "result" assoc with
    | Some _ -> true
    | None -> false
end

(** The module for request object *)
module Request = struct
  type t = {_method: string; params: json option; id: id option}

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
end

module Response = struct
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

  let of_json js =
    match js with
    | `Assoc assoc -> begin
        if is_response_success assoc || is_response_error assoc then
          let id = List.assoc_opt "id" assoc
          and error = List.assoc_opt "error" assoc
          and result = List.assoc_opt "result" assoc in
          let id = match id with
            | Some id -> Some (Int64.of_string @@ Util.to_string id)
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
end

(**/**)
(* ignore ocamldoc below *)

module Test = struct

  let tests = [
    "should be able to parse response object from json", (fun () ->
        let json = {|
{"jsonrpc": "2.0", "result": 19, "id": "1"}
|}
        in
        let expected = Response.{id = Some 1L; result = Some (`Int 19); error = None} in
        Response.of_json (Yojson.Basic.from_string json) = Ok expected
      );

    "should be able to parse error response object from json", (fun () ->
        let json = {|
{"jsonrpc": "2.0", "error": {"code": -32700, "message": "error happenned"}, "id": "1"}
|}
        in
        let expected = Response.{
            id = Some 1L;
            result = None;
            error = Some Error.{code = Error_code.Parse_error; message = "error happenned"; data = None}
          } in
        Response.of_json (Yojson.Basic.from_string json) = Ok expected
      );

    "should be able to parse error response object with data from json", (fun () ->
        let json = {|
{"jsonrpc": "2.0", "error": {"code": -32700, "message": "error happenned", "data": "detail"}, "id": "1"}
|}
        in
        let expected = Response.{
            id = Some 1L;
            error = Some Error.{code = Error_code.Parse_error; message = "error happenned"; data = Some (`String "detail")};
            result = None;
          } in
        Response.of_json (Yojson.Basic.from_string json) = Ok expected
      );

    "should be able to encode request object to json", (fun () ->
        let expected_json = {|
{"jsonrpc": "2.0", "params": [1,2,3], "method": "sum", "id": "2"}
|}
        in
        let expected_json = from_string expected_json in
        let request = Request.{id = Some 2L; _method = "sum"; params = Some (`List [`Int 1;`Int 2;`Int 3]) } in
        (* sort key to compare with (=)  *)
        let actual = Yojson.Basic.sort @@ Request.to_json request
        and expected = Yojson.Basic.sort expected_json in
        actual = expected
      );

  ]

end
