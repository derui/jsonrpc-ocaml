
type t = {
  result : Yojson.Basic.json option;
  id : Types.id option;
  error : Types.Error.t option;
}

val to_json : t -> [> `Assoc of (string * Yojson.Basic.json) list ]
val of_json : Yojson.Basic.json -> (t, Errors.t) result

module Test : sig
  val tests : (string * (unit -> bool)) list
end
