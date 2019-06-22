module type Json_type = sig
  type t
end

module Make_client
    (J : Json_type)
    (Err : Error.S with type json := J.t)
    (Req : Request.S with type json := J.t)
    (Res : Response.S with type json := J.t and type error = Err.t)
    (T : Types.Thread) =
struct
  type json = J.t

  module Response = Res
  module Request = Req
  module Error = Err

  (** The signature to define interface of an API *)
  type ('p, 'r) api_def =
    { _method : string  (** Return method of RPC *)
    ; params_to_json : 'p -> json  (** convert parameter to json *)
    ; result_of_json : json -> 'r  (** convert json to result when request success. *) }

  (** A module signature to create request for JSON-RPC. Using this module combines with a module implemented
      Rpc module interface. *)
  module type S = sig
    type json = J.t

    val call : api:('p, 'r) api_def -> ?params:'p -> unit -> ('r option, Err.t) result T.t
    (** Make a request for API *)

    val notify : api:('p, _) api_def -> ?params:'p -> unit -> unit T.t
    (** Make a notification for API *)
  end

  (** A module contains generic helper function for Client implementation *)
  module Helper = struct
    (** [setup_request ~api ~params] return partial initialized request. User should manage ID in the request *)
    let setup_request (type p r) ~(api : (p, r) api_def) ~(params : p option) =
      let params =
        match params with Some params -> Some (api.params_to_json params) | None -> None
      in
      Request.{id = None; params; _method = api._method}
  end
end
