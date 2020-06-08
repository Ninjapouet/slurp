(** Slurp Cohttp server definitions.

    {1 Quickstart}

    The Slurp cohttp implementation is based on {!Ezcmdliner} library and quite
    straightforward. It simply adds some options (not enough yet though) to
    configure cohttp and simply calls the {!Slurp.Route.eval} function.

    Basically, there are two ways of using the Slurp cohttp server depending
    on extension needness. If you don't need to add custom
    features to the server definition, simply use the [slurpd-cohttp.server]
    binary.

    If you're not satisfied with the default configuration options proposed or
    if you want to extend or rewrote the server, you can use the API below
    to do it.

    For example, if you want the server to listen another port you can do
    the following stuff. First, retrieve the server configuration to inherit
    its command line:
    {[
      let cfg = Slurp_cohttp.Server.cfg
    ]}
    Then, add the command line options. Here, we add an alt port option:
    {[
      let alt_port = Ezcmdliner.(register @@ value @@ opt
        ~doc:"Alternative listening port."
        ~conv:int
        ~default:3000
        ["alt-port"])
    ]}
    This adds the option to the configuration [cfg]. See the {!Ezcmdliner}
    documentation for more informations. The registering action returns
    a getter to the option value (typically [unit -> 'a] where ['a]
    depends on the converter [conv], here an [int]). Then you can
    extend or redefine the server definition:
    {[
      let command = Ezcmdliner.({
          Slurp_cohttp.Server.command with
          cmd = fun () -> Lwt.join [
              Slurp_cohttp.Server.command.cmd (); (* Legacy listening on port *)
              (fun () -> Slurp_cohttp.Server.server ~port:(alt_port ())); (* Listening on alt port *)
        })
    ]}


    {1 API}

*)

open Ezcmdliner

(** The Slurp cohttp configuration. *)
val cfg : cfg

(** Returns the port value. *)
val port : unit -> int

(** Returns the services to load. *)
val services : unit -> string list

(** The underlying server function. *)
val server :
  ?mode:Conduit_lwt_unix.server ->
  ?callback:(Cohttp_lwt_unix.Server.conn ->
             Cohttp.Request.t ->
             Cohttp_lwt.Body.t ->
             Cohttp_lwt_unix.Server.response_action Lwt.t) ->
  unit -> unit Lwt.t

(** The Slurp cohttp command specification. *)
val command : unit Lwt.t command
