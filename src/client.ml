open Types
include Client_intf

(** Make a request and response handler that will use with response which has same id of the request it. *)
let make_request
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
  let request = Request ({id; params; _method = A.name}) in
  (id, request, handler')

(**/**)
(* ignore document below *)

module Test = struct
  let tests = [
    ("should be able to wrap a request with API definition", fun _ ->
        let module A = struct
          type params = int list
          type result = int

          let name = "sum"
          let params_to_json = function
            | None -> None
            | Some v -> Some (`List (List.map (fun v -> `Int v) v))
          let result_of_json = function
            | `Int v -> v
            | _ -> failwith ""
        end in

        let id, req, handler = make_request (module A) (Some [1;2;3]) ignore in
        let expected = Request {id = id; params = Some (`List [`Int 1;`Int 2;`Int 3]); _method = "sum"} in
        expected = req
    );
  ]
end
