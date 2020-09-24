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

class cohttp = object(self)

  inherit [_] Clim.cli

  val mutable port = 80
  method port_names = ["p"; "port"]

  val mutable services = []
  method services_names = ["s"; "services"]

  val mutable static_dirs = []
  method static_dirs_names = ["static"]

  val mutable static_prefix = "static"
  method static_prefix_names = ["static-prefix"]

  initializer
    let open Clim in
    (* port option *)
    self#arg
        (value @@ opt
           ~doc:"The server listening port."
           int
           port
           self#port_names)
        (fun p -> port <- p);

    (* services option *)
    self#arg
      (value @@ opt_all
         ~docv:"SERVICES"
         ~doc:"Loads external services $(docv)."
         (list string)
         []
         self#services_names)
      (fun l -> services <- List.concat l);

    (* static dirs option *)
    self#arg
      (value @@ opt_all
         ~docv:"DIRS"
         ~doc:"Serve files in $(docv)."
         (list dir)
         []
         self#static_dirs_names)
      (fun l -> static_dirs <- List.concat l);

    (* static prefix option *)
    self#arg
      (value @@ opt
         ~docv:"NAME"
         ~doc:"Static file prefix to use."
         string
         "static"
         self#static_prefix_names)
      (fun s -> static_prefix <- s)

  method callback _ _ _ = Lwt.fail_with "callback"

  method entrypoint () =
    let mode = `TCP (`Port port) in
    let conn_closed (_, _c) =
      (* Fmt.pr "connection %a closed@." Sexplib.Sexp.pp (Cohttp.Connection.sexp_of_t c) *)
      () in
    List.iter (fun path -> match Dynlink.loadfile path with
        | () -> ()
        | exception Dynlink.Error (Dynlink.Cannot_open_dynamic_library _) ->
          (* the related exception is platform specific so, to ensure some determinism,
             we regroup all underlying errors into the same message. *)
          Fmt.epr "[error] cannot open %s@." path
        | exception e ->
          Fmt.epr "[error] while loading %s: %a@."
            path
            Fmt.exn e) services;
    Tools.static ~prefix:static_prefix static_dirs;%lwt
    Server.create
      ~mode
      (Server.make_response_action
         ~callback:(handle self#callback)
         ~conn_closed ())

end
