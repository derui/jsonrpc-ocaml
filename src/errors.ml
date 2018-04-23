open Yojson.Basic

type t =
  (** The exception thrown when toplevel json is null. *)
    Empty_json
  (** Invalid object on parse *)
  | Invalid_object of json
  | Invalid_request
  | Invalid_response
  | Not_found_version
