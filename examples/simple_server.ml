module Y = Jsonrpc_yojson

let jsonrpc_server =
  let server = Y.Server.make () in
  Y.Server.expose ~_method:"sample"
    ~handler:(function None -> Lwt.return_ok None | Some req -> Lwt.return_ok @@ Some req)
    server

let handler _ (req : Cohttp_lwt_unix.Request.t) (body : Cohttp_lwt.Body.t) =
  let uri = Cohttp.Request.uri req in
  match Uri.path uri with
  | "/" ->
      let%lwt json = Cohttp_lwt.Body.to_string body in
      Printf.printf "%s\n%!" json ;
      let request =
        match Yojson.Safe.from_string json |> Y.Request.of_json with
        | Ok v -> v
        | Error _ -> failwith "fail"
      in
      let%lwt res = Y.Server.handle_request ~request jsonrpc_server in
      let%lwt resp =
        Cohttp_lwt_unix.Server.respond_string ~status:`OK
          ~body:(Y.Response.to_json res |> Yojson.Safe.to_string)
          ()
      in
      Lwt.return (`Response resp)
  | _ ->
      let%lwt resp =
        Cohttp_lwt_unix.Server.respond_string ~status:`Not_found
          ~body:(Sexplib.Sexp.to_string_hum (Cohttp.Request.sexp_of_t req))
          ()
      in
      Lwt.return (`Response resp)

let () =
  let port = 8081 in
  Lwt_main.run
  @@ Cohttp_lwt_unix.Server.create
       ~mode:(`TCP (`Port port))
       (Cohttp_lwt_unix.Server.make_response_action ~callback:handler ())
