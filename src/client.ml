open Types
include Client_intf

(** Make a request and response handler that will use with response which has same id of the request it. *)
let make_request
    (type p) (type r)
    (module A: Api_def with type params = p and type result = r)
    (params: p option)
    (handler : (r, Error.t) result -> unit) =
  let params = A.params_to_json params in
  let handler' v =
    let param = match (v.Response.result, v.Response.error) with
      | _, Some e -> Error e
      | Some r, _ -> Ok (A.result_of_json r)
      | _ -> failwith "Unknown response"
    in
    handler param
  in
  let id = Random.int64 Int64.max_int in
  let request = Request.{id = Some id; params; _method = A.name} in
  (request, Some handler')

(** Make a request for notification. *)
let make_notification
    (type p) (type r)
    (module A: Api_def with type params = p and type result = r)
    (params: p option) =
  let params = A.params_to_json params in
  let request = Request.{id = None; params; _method = A.name} in
  (request, None)

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

        let req, handler = make_request (module A) (Some [1;2;3]) ignore in
        let expected = Request.{id = req.Request.id; params = Some (`List [`Int 1;`Int 2;`Int 3]); _method = "sum"} in
        expected = req
    );
    ("should be able to wrap a notification with API definition", fun _ ->
        let module B = struct
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

        let req, _ = make_notification (module B) (Some [1;2;3]) in
        let expected = Request.{id = None; params = Some (`List [`Int 1;`Int 2;`Int 3]); _method = "sum"} in
        expected = req
    );
  ]
end
