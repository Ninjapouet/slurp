(** Slurp Cohttp server definitions.

    {1 Quickstart}

    To use slurp with the cohttp backend, you can simply call the
    [slurp-cohttp.server] binary coming with the package. However, if you
    need to tune the server implementation, you can use the server
    extensible definition inherited from {!Clim.cli}.

    {1 Extension}

    Most of the time, the server is launched together with other services
    that may already bind the slurp default option names. To avoid any
    conflict, the server CLI is defined from {!Clim.cli} and option names
    are overloadable methods.

    The parameters names are defined with string lists
    following the {{:https://erratique.ch/logiciel/cmdliner}Cmdliner}
    convention: short names are one char strings and others strings are long
    names. For example, if you want to rename the "--port" option,
    simply reinstanciate the server definition with a custom configuration:
    {[
      open Slurp_cohttp

      let my_server = object
        inherit cohttp
        method! port_names = ["my-port"]
      end

      let _ = Lwt_main.run my_server#run
    ]}

    the other extension oftenly used is adding new options for various
    services added with the custom REST API. Adding options is
    allowed by the {{:https://github.com/Ninjapouet/clim}Clim}
    interface. For example, we can add the option [foo] simply by
    registering it with:
    {[
      let my_server = object
         ...
         val foo = 42
         initializer
           self#arg Clim.(value @@ opt
            ~doc:"My awesome option"
            int
            42
            ["foo"]) (fun v -> foo <- v)
         ...
      end
    ]}
    [foo] will then contain the [foo] option value at runtime.

    More subtle extensions are also available by redefining the
    [My_server.command] value to add some custom behavior to the resulting
    binary. For example:
    {[
      let my_server = object(self)
        inherit cohttp as super
        ...
        method! entrypoint () =
           Lwt.join [
             super#entrypoint ();
             (fun () -> print_endline "Hello there!"; Lwt.return_unit);
           ]
        ...
    ]}
    will add the trace "Hello there!" on server run.

    {1 API}
*)

(** A customizable SLURP cohttp definition. *)
class cohttp : object
  inherit [_] Clim.cli

  (** The listening port *)
  val port : int

  (** SLURP services to load. *)
  val services : string list

  (** Static directories to serve. *)
  val static_dirs : string list

  (** Prefix used to serve static content. *)
  val static_prefix : string

  (** Following methods define the corresponding CLI option names. *)
  method port_names : string list
  method services_names : string list
  method static_dirs_names : string list
  method static_prefix_names : string list

  (** Callbakc called before the SLURP routing if any operation must be
      done before. *)
  method callback :
    Cohttp_lwt_unix.Server.conn ->
    Cohttp_lwt_unix.Request.t ->
    Cohttp_lwt.Body.t ->
    Cohttp_lwt_unix.Server.response_action Lwt.t

  (** Configure the server mode. By default, it uses
      TCP on specified port. *)
  method mode : Conduit_lwt_unix.server

  (** Launches the SLURP cohttp server. *)
  method entrypoint : unit -> unit Lwt.t
end
