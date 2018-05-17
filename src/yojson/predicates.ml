

let has_error_code assoc = match List.assoc_opt "code" assoc with
  | Some (`Int _) -> true
  | _ -> false

let has_error_message assoc = match List.assoc_opt "message" assoc with
  | Some (`String _) -> true
  | _ -> false

let is_error = function
  | `Assoc assoc -> begin
      has_error_code assoc && has_error_message assoc
    end
  | _ -> false

let has_version assoc = List.assoc_opt "jsonrpc" assoc = Some (`String "2.0")
let has_params assoc = match List.assoc_opt "params" assoc with
  | Some _ -> true
  | None -> false

let has_id assoc = match List.assoc_opt "id" assoc with
  | Some (`String _) -> true
  | _ -> false

let has_nullable_id assoc = match List.assoc_opt "id" assoc with
  | Some (`String _) | Some `Null -> true
  | _ -> false

let has_method assoc = match List.assoc_opt "method" assoc with
  | Some (`String _) -> true
  | _ -> false

let has_error assoc = match List.assoc_opt "error" assoc with
  | Some err -> is_error err
  | None -> false

let has_result assoc = match List.assoc_opt "result" assoc with
  | Some _ -> true
  | None -> false
