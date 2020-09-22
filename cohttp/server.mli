(** Slurp Cohttp server definitions.

    {1 Quickstart}

    To use slurp with the cohttp backend, you can simply call the
    [slurp-cohttp.server] binary coming with the package. However, if you
    need to tune the server implementation, you can use the server
    extensible definition.

    {1 Extension}

    Most of the time, the server is launched together with other services
    that may already bind the slurp default option names. To avoid any
    conflict, the server comes with a functorial definition based on
    option names. The parameters names are defined with string lists
    following the {{:https://erratique.ch/logiciel/cmdliner}Cmdliner}
    convention: short names are one char strings and others strings are long
    names. For example, if you want to rename the "--port" option,
    simply reinstanciate the server definition with a custom configuration:
    {[
      open Slurp_cohttp

      module My_configuration = struct
        let port = ["my-port"]
        let service = Default.service
      end

      module My_server = Make(My_configuration)

      let _ = Ezcmdliner.run My_server.command
    ]}

    the other extension oftenly used is adding new options for various
    services added with the custom REST API. Adding options is
    allowed by the {{:https://github.com/Ninjapouet/ezcmdliner}Ezcmdliner}
    interface. For example, we can add the option [foo] simply by
    registering it with:
    {[
      let foo = Ezcmdliner.(
          register My_server.cfg @@ value @@ opt
            ~doc:"My awesome option"
            int
            42
            ["foo"])
    ]}
    [foo ()] will then return the [foo] option value at runtime.

    More subtle extensions are also available by redefining the
    [My_server.command] value to add some custom behavior to the resulting
    binary. For example:
    {[
      let command = Ezcmdliner.({
          My_server.command with
          cmd = fun () -> Lwt.join [
              My_server.command.cmd (); (* Legacy server commmand *)
              (fun () -> print_endline "Hello there!"; Lwt.return_unit);
          ];
        })
    ]}
    will add the trace "Hello there!" on server run.

    {1 API}
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

(** Default configuration. *)
module Default : CFG

(** Cohttp slurp functor. *)
module Make (C : CFG) : S

(** {1 Default implementation}

    This library gives a default implementation using the "p" or "port"
    names for the port option and "s" or "service" for the services option. *)

include S
