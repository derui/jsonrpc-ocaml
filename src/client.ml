module type Json_type = sig
  type t
end

(** The signature to define interface of an API *)
module type Api_def = sig
  type params

  type result

  type json

  val name : string
  (** Return name of method *)

  val params_to_json : params -> json
  (** convert parameter to json *)

  val result_of_json : json -> result
  (** convert json to result when request success. *)
end

(** A module signature to create request for JSON-RPC. Using this module combines with a module implemented
    Rpc module interface. *)
module type S = sig
  type json

  module Thread : Rpc.Thread

  module Error : Error.S with type json = json

  val call :
    api:(module
          Api_def
          with type params = 'p and type result = 'r and type json = json)
    -> ?params:'p
    -> unit
    -> ('r option, Error.t) result Thread.t
  (** Make a request for API *)

  val notify :
    api:(module
          Api_def
          with type params = 'p and type result = 'r and type json = json)
    -> ?params:'p
    -> unit
    -> unit Thread.t
    (** Make a notification for API *)
end

module Make (T : Json_type) (R : Rpc.S with type json = T.t) :
  S with type json := T.t and module Thread := R.Thread = struct
  type json = T.t

  module Error = R.Response.Error

  (** Make a request and response handler that will use with response which has same id of the request it. *)
  let call (type p r)
      ~api:(module A : Api_def
             with type params = p and type result = r and type json = json)
      ?(params : p option) () =
    let params =
      match params with
      | Some params -> Some (A.params_to_json params)
      | None -> None
    in
    let id = Random.int64 Int64.max_int in
    let request = R.Request.{id= Some id; params; _method= A.name} in
    let ( >>= ) = R.Thread.bind in
    R.call request
    >>= fun r ->
    let ret =
      match (r.R.Response.result, r.R.Response.error) with
      | _, Some e -> Error e
      | Some r, _ -> Ok (Some (A.result_of_json r))
      | None, _ -> Ok None
    in
    R.Thread.return ret

  (** Make a request for notification. *)
  let notify (type p r)
      ~api:(module A : Api_def
             with type params = p and type result = r and type json = json)
      ?(params : p option) () =
    let params =
      match params with
      | Some params -> Some (A.params_to_json params)
      | None -> None
    in
    let request = R.Request.{id= None; params; _method= A.name} in
    R.notify request
end
