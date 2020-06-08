open Cohttp
open Cohttp_lwt_unix
open Slurp

let handle other conn req body =
  match%lwt other conn req body with
  | resp -> Lwt.return resp
  | exception _ ->
    let resource = Request.resource req in
    let%lwt body = body |> Cohttp_lwt.Body.to_string in
    begin match Request.meth req with
    | (`GET|`POST) as meth ->
      begin match%lwt Route.eval meth resource body with
        | `Data (data, mime) ->
          let headers =
            let open Header in
            let headers = init () in
            add headers "content-type" mime in
          let%lwt resp, body = Server.respond_string
              ~status:`OK
              ~headers
              ~body:Body.(to_string (`String data)) () in
          Lwt.return @@ `Response (resp, body)
      end
    | meth ->
      let%lwt resp, body = Server.respond_error
          ~status:`Not_implemented
          ~body:Body.(to_string (`String (Code.string_of_method meth)))
          () in
      Lwt.return @@ `Response (resp, body)
    end

let cfg = Ezcmdliner.create ()

let port = Ezcmdliner.(
    register cfg @@ value @@ opt
      ~doc:"The server listening port."
      int
      80
      ["p"; "port"])

let services =
  let get = Ezcmdliner.(
      register cfg @@ value @@ opt_all
        ~docv:"SERVICES"
        ~doc:"Loads external services $(docv)."
        (list file)
        []
        ["s"; "services"]) in
  let l_get () = lazy (List.concat (get ())) in
  fun () -> Lazy.force (l_get ())


let static_dirs =
  let get = Ezcmdliner.(
      register cfg @@ value @@ opt_all
        ~docv:"DIRS"
        ~doc:"Serve files in $(docv)."
        (list dir)
        []
        ["static"]) in
  let l_get () = lazy (List.concat (get ())) in
  fun () -> Lazy.force (l_get ())

let static_prefix = Ezcmdliner.(
    register cfg @@ value @@ opt
      ~docv:"NAME"
      ~doc:"Static file prefix to use."
      string
      "static"
      ["static-prefix"])

let name = Filename.basename Sys.executable_name

let default _ _ _ = Lwt.fail_with "callback"

let server ?(mode = `TCP (`Port (port()))) ?(callback = default) () =
  let conn_closed (_, c) =
    Fmt.pr "connection %a closed@." Sexplib.Sexp.pp (Cohttp.Connection.sexp_of_t c) in
  List.iter (fun path -> match Dynlink.loadfile path with
      | () -> ()
      | exception e ->
        Fmt.epr "[%s] error while loading %s: %a@."
          name
          path
          Fmt.exn e) (services ());
  Tools.static ~prefix:(static_prefix ()) (static_dirs ());%lwt
  Server.create
    ~mode
    (Server.make_response_action
       ~callback:(handle callback)
       ~conn_closed ())

let command = Ezcmdliner.command ~cfg (server)
