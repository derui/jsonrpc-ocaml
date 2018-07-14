
type id = int64

(** Type type of error code defined by specification of JSON RPC *)
module Error_code = struct
  type t =
    | Parse_error
    | Invalid_request
    | Method_not_found
    | Invalid_params
    | Internal_error
    | Server_error of int
    | Application_error of string * int

  let make ~message = function
    | -32700 -> Parse_error
    | -32600 -> Invalid_request
    | -32601 -> Method_not_found
    | -32602 -> Invalid_params
    | -32603 -> Internal_error
    | _ as v when -32000 <= v && v <= -32099 -> Server_error v
    | _ as v -> Application_error (message, v)

  let to_int = function
    | Parse_error -> -32700
    | Invalid_request -> -32600
    | Method_not_found -> -32601
    | Invalid_params -> -32602
    | Internal_error -> -32603
    | Server_error v -> v
    | Application_error (_, v) -> v

  let to_message = function
    | Parse_error -> "Parse error"
    | Invalid_request -> "Invalid Request"
    | Method_not_found -> "Method not found"
    | Invalid_params -> "Invalid params"
    | Internal_error -> "Internal error"
    | Server_error _ -> "Server error"
    | Application_error (v, _) -> v
end

module Parse_error = struct
  type 'a t =
    (* The exception thrown when toplevel json is null. *)
      Empty_json
    (* Invalid object on parse *)
    | Invalid_object of 'a
    | Invalid_request
    | Invalid_response
    | Not_found_version

end
