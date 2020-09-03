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


    {1 Functorial interface}

    {2 Configuration}

    The server implementation is parametrized by command line parameter names in order
    to make it usable in more complex binaries without messing up command line options.
    The parameters names are defined in {!CFG} with string lists following the
    {!Cmdliner} convention : short names are one char strings and others strings are long
    names.
*)

open Ezcmdliner

(** Command line parameter names. *)
module type CFG = sig
  (** Port option names. *)
  val port : string list

  (** Service option names. *)
  val service : string list
end

(** {2 Interface}

    The cohhtp slurp interface is given by {!S} which give the {!Ezcmdliner}
    configuration used which allows adding more parameters if needed. It also
    gives the parameters accessors and an underlying server
    implementation which, again, is configurable through specific cohttp
    stuff.
*)

(** The cohttp slurp interface. *)
module type S = sig

  (** The {!Ezcmdliner} configuration. *)
  val cfg : cfg

  (** Returns the port value. *)
  val port : unit -> int

  (** Returns the services to load. *)
  val services : unit -> string list

  (** [server ~mode ~callback ()] launches the cohttp slurp server using the
      {!Conduit} mode [mode]. [callback] is used to overwrite the default
      slurp behavior to handle some corner case. If [callback] raises some
      exception, the default slurp behavior is used. *)
  val server :
    ?mode:Conduit_lwt_unix.server ->
    ?callback:(Cohttp_lwt_unix.Server.conn ->
               Cohttp.Request.t ->
               Cohttp_lwt.Body.t ->
               Cohttp_lwt_unix.Server.response_action Lwt.t) ->
    unit -> unit Lwt.t

  (** The slurp cohttp command specification. *)
  val command : unit Lwt.t command

end

(** Cohttp slurp functor. *)
module Make (C : CFG) : S

(** {1 Default implementation}

    This library gives a default implementation using the "p" or "port"
    names for the port option and "s" or "service" for the services option. *)

include S
