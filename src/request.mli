open Types

(** The module handle request object *)
type t = {
  _method : string;
  params : Yojson.Basic.json option;
  id : id option;
}
val to_json : t -> Yojson.Basic.json
val of_json : Yojson.Basic.json -> (t, Errors.t) result

module Test : sig
  val tests : (string * (unit -> bool)) list
end
