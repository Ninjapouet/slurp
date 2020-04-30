open Cohttp
open Cohttp_lwt_unix
open Slurp



let _ =
  Route.(get
           ~id:"print"
           ~path:Path.(path "print" /: string "msg" /? unit --> string "result")
           (fun s () ->
              Fmt.pr "%s@." s;
              s))

let callback _conn req body =
  let resource = Request.resource req in
  let%lwt body = body |> Cohttp_lwt.Body.to_string in
  let%lwt response = match Request.meth req with
    | (`GET|`POST) as meth -> Route.eval meth resource body
    | _ -> Lwt.fail_with "method" in
  match response with
  | `Data data ->
    Server.respond_string
      ~status:`OK
      ~body:Body.(to_string (`String data)) ()

let cfg = Ezcmdliner.create ()
let port = Ezcmdliner.(register cfg @@ value @@ opt ~default:80 ~conv:int ["p"; "port"])

let services =
  let get = Ezcmdliner.(
      register cfg @@ value @@
      opt_all ~default:[] ~conv:(list file) ["s"; "services"]) in
  fun () -> List.concat (get ())

let name = Filename.basename Sys.executable_name

let server ~port =
  List.iter (fun path -> match Dynlink.loadfile path with
      | () -> ()
      | exception e ->
        Fmt.epr "[%s] error while loading %s: %a@."
          name
          path
          Fmt.exn e) (services ());
  Server.create
    ~mode:(`TCP (`Port port))
    (Server.make
       ~callback
       ())

let command = Ezcmdliner.command ~cfg (fun () -> server ~port:(port()))
