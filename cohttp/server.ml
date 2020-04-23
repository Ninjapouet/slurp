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
  let response = match Request.meth req with
    | (`GET|`POST) as meth -> Route.eval meth resource body
    | _ -> failwith "method" in
  match response with
  | `Data data ->
    Server.respond_string
      ~status:`OK
      ~body:Body.(to_string (`String data)) ()

type t = {

  port : int;
  [@default 80]
  [@env "SLURP_PORT"]
  [@docv "PORT"]
  (** Sets the server port to use *)

  services : string list;
  [@env "SLURP_SERVICES"]
  [@docv "SERVICES"]
  (** Sets the compiled modules defining interfaces *)
}
[@@deriving cmdliner]
[@@doc "Cohttp server for Slurp services."]

let name = Filename.basename Sys.executable_name

let server t =
  List.iter (fun path -> match Dynlink.loadfile path with
      | () -> ()
      | exception e ->
        Fmt.epr "[%s] error while loading %s: %a@."
          name
          path
          Fmt.exn e) t.services;
  Lwt_main.run (
    Server.create
      ~mode:(`TCP (`Port t.port))
      (Server.make
         ~callback
         ()))

let () = cmdliner server
