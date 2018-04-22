
open Yojson.Basic

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

end

(** The type of error object *)
type error = {
  code: Error_code.t;
  message: string;
  data: json option;
}

(* All structures for JSON RPC *)
type jsonrpc =
  | Request of {_method: string; params: json option; id: int64}
  | Response_success of {result: json; id: int64}
  | Response_error of {error: error; id: int64}
  | Notification of {_method: string; params: json option}
