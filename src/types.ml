
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
