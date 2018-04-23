open Types
include Client_intf

(** Make a request and response handler that will use with response which has same id of the request it. *)
let wrap_request
    (type p) (type r)
    (module A: Api_def with type params = p and type result = r)
    (params: p option)
    (handler : (r, error) result -> unit) =
  let params = A.params_to_json params in
  let handler' v =
    let param = match v with
      | Response_error e -> Error e.error
      | Response_success v -> Ok (A.result_of_json v.result)
      | _ -> failwith "Unknown response"
    in
    handler param
  in
  let id = Random.int64 Int64.max_int in
  let request = jsonrpc_to_json (Request ({id; params; _method = A.name})) in
  (request, handler')
