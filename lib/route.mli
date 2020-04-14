(** Routing operations.

    This module is the main part of the SLURP library. It defines
    how to build routes and provides tools to use them into
    custom servers.
*)

 (* SLURP stands
  *    for Sinatra Like URL Route Processing to fake seriousness but
  *    it reality, it's some kind of onomatopoeia joke since "route"
  *    or "rest" or other Sinatra related words were already taken. *)


(** {1 Quick start}

    Just define your operations with a Sinatra like style. For example,
    if you want to export a [sum] function that sums its [int]
    parameter [x] and [y], just type:
    {[
    # open Slurp;;
    # Route.(get
              ~id:"sum"
              ~path:Path.(path "sum" /: int "x" /: int "y" /? unit --> int "result")
              (fun x y () -> x + y));;
    - : unit = ()
    ]}
    [get] tells that the function is a GET method in sense of HTTP. If your
    server isn't a HTTP one, pick whatever [get], [post] or other method as you
    like. [id] parameter names the function for documentation or some export
    feature (OpenAPI, command line, ...). [path] gives the Sinatra like route with
    naming conventions quite... well... conventional, except that we added
    the body and response specification into the route definition. This allows
    the whole stuff to be statically typechecked and avoid coding
    errors as far as possible.

    When your are satisfied with your API, simply define a server interface
    using the {!SERVER} module type and give it to the {!make} function:
    {[
    # let response = Route.make my_server;;
    - : string event
    ]}
    It returns a simple \[Lwt_\]react event corresponding to the responses
    answered by the proper route selection according to the server events.

    {1 Details}

    The route module is designed to be modular and not bound to a server
    definition. This way, anybody can define its REST API and use its own
    mean to serve it (Cohttp, H2, Apache, Nginx or whatever).

    Routes are strongly typed to avoid design issues and
    ensures the maximum safety as far as possible. Route templating is
    done by specifying parameters using the {!spec} type.

    {2 Specification}

    This {!spec} type allows to attach informations to a parameter, a
    body or response for the typechecking but also for exporting
    purpose. See the related definition to know what kind of information
    can be attached.

    A ['a spec] stands for a parameter, body or response that maps to
    a ['a] OCaml type.
*)

open Lwt_react

(** The specification type. *)
type 'a spec

(** Unit specification. It has the same semantical value of the OCaml
    unit value so that naming or description is useless. *)
val unit : unit spec

(** [int ~description name] defines a [int] spec with [description] and
    name it by [name]. *)
val int : ?description:string -> string -> int spec

(** [string ~description name] defines a [string] spec with [description] and
    name it by [name]. *)
val string : ?description:string -> string -> string spec

(** [json scheme ~description name] defines a [json] spec with [description]
    and name it by [name]. Values matching this specification must match the
    JSON [scheme]. *)
val json : Ezjsonm.value -> ?description:string -> string -> Ezjsonm.value spec

(** [html ~description name] defines a [string] spec with [description] and
    name it by [name]. *)
val html : ?description:string -> string -> string spec

(** See {!spec}. *)
type 'a parameter = 'a spec

(** See {!spec}. *)
type 'a body = 'a spec

(** See {!spec}. *)
type 'a response = 'a spec

(** {2 Paths}

    Paths are route selector definition. It basically follows the usual
    route definition with paths, templates and so on. *)

(** The path type. The firs type parameter stand for the resulting function
    type. The second is the body type (if used) and the third stands for
    the response type. *)
type ('a, 'b, 'c) path

(** The query type is just used to distinguish path and query parts of the whole
    path specification. *)
type ('a, 'b, 'c) query

(** Path pretty printer. *)
val pp_path : ('a, 'b, 'c) path Fmt.t

(** {2 Operators}

    To defines paths, we use the operators given in {!Path}. They have quite a
    complex type but don't bother with it. Simply keep in mind that operators
    ending with "/" stands for exact match section definition. Those
    ending with "/:" is a parameter definition (or template in
    Sinatra vocabulary). Those ending with "/?" introduce the query part.
    One star "*" operators introduce the any section match, two stars "**" are the
    any section match on zero or more levels.
    The "+" operators adds query parameters and the final "-->" indicates the
    body and responses specification. *)

(** Path operators. *)
module Path : sig

  (** [path s] starts a path with the given root [s]. To defines an empty
      root prefix simply use "". *)
  val path : string -> ('a, 'b, 'c) path -> ('a, 'b, 'c) path

  (** [a / b] matches the path [a] then [b]. *)
  val ( / ) :
    (('a, 'b, 'c) path -> ('d, 'b, 'c) path) -> string -> ('a, 'b, 'c) path ->
    ('d, 'b, 'c) path

  (** [a /*/ b] matches the path [a] then any section and then [b]. *)
  val ( /*/ ) :
    (('a, 'b, 'c) path -> ('d, 'b, 'c) path) -> string -> ('a, 'b, 'c) path ->
    ('d, 'b, 'c) path

  (** [a /**/ b] matches the path [a] then any sections (if any) and then [b]. *)
  val ( /**/ ) :
    (('a, 'b, 'c) path -> ('d, 'b, 'c) path) -> string -> ('a, 'b, 'c) path ->
    ('d, 'b, 'c) path

  (** [a /: b] matches the path [a] then the bind the [b] to its future
      instanciation. *)
  val ( /: ) :
    (('a -> 'b, 'c, 'd) path -> ('e, 'c, 'd) path) -> 'a parameter -> ('b, 'c, 'd) path ->
    ('e, 'c, 'd) path

  (** [a /*/: b] matches the path [a], then any section and then bind the [b] to
      its future instanciation. *)
  val ( /*/: ) :
    (('a -> 'b, 'c, 'd) path -> ('e, 'c, 'd) path) -> 'a parameter -> ('b, 'c, 'd) path ->
    ('e, 'c, 'd) path

  (** [a /**/: b] matches the path [a], then any sections (if any) and then bind
      the [b] to its future instanciation. *)
  val ( /**/: ) :
    (('a -> 'b, 'c, 'd) path -> ('e, 'c, 'd) path) -> 'a parameter -> ('b, 'c, 'd) path ->
    ('e, 'c, 'd) path

  (** [a /? b] matches the path [a] then the query part [b]. *)
  val ( /? ) :
    (('a, 'b, 'c) path -> ('d, 'b, 'c) path) -> 'e ->
    (('a, 'b, 'c) query -> ('d, 'b, 'c) path) * 'e

  (** [a /*/? b] matches the path [a], then any section and then the query
      part [b]. *)
  val ( /*/? ) :
    (('a, 'b, 'c) path -> ('d, 'b, 'c) path) -> 'e ->
    (('a, 'b, 'c) query -> ('d, 'b, 'c) path) * 'e

  (** [a /**/? b] matches the path [a], then any sections (if any) and then
      the query part [b]. *)
  val ( /**/? ) :
    (('a, 'b, 'c) path -> ('d, 'b, 'c) path) -> 'e ->
    (('a, 'b, 'c) query -> ('d, 'b, 'c) path) * 'e

  (** [a + b] adds [a] to query parameters of [b]. *)
  val ( + ) :
    (('b -> 'c, 'd, 'e) query -> 'f) * 'b parameter ->
    'a parameter -> (('c, 'd, 'e) query -> 'f) * 'a parameter

  (** [a --> b] finalize the path specification and adds [a] as the body
      specification and [b] as the response specification. *)
  val ( --> ) :
    (('b -> 'c, 'b, 'c) query -> ('d, 'b, 'c) path) * 'b body ->
    'c response -> ('d, 'b, 'c) path

end

(** {2 Routes}

    Routes are defined by following the HTTP methods (GET, POST and so on)
    as Sinatra is made for it.
    However, this library takes some distance with it and, while routes are
    defined using the same methods to follow the Sinatra model, one can use
    whatever route method as long as it fills its need if the
    underlying server doesn't use HTTP.
*)

(** [get ~path ~id ~description f] defines the GET route using [path] and
    associates the [id] and [description] to it. The function called
    when the route is resolved is [f]. This route definition doesn't
    use the body part as recommended by the HTTP specification. *)
val get : path:('a, unit, 'b) path -> id:string -> ?description:string -> 'a -> unit

(** [post ~path ~id ~description f] defines the POST route using [path] and
    associates the [id] and [description] to it. The function called
    when the route is resolved is [f]. *)
val post : path:('a, 'b, 'c) path -> id:string -> ?description:string -> 'a -> unit

(** {1 Server definition}

    The route module doesn't define its own server implementation in order
    to be as modular as possible. In order to serve the API defined with
    {!Route}, one must define a {!SERVER} module.

    The particularity of
    of this module is that it doesn't need abstract function interface
    for defining such a generic interface is a complicated problem. To
    avoid this issue, SLURP uses an event based interface having the
    extremely good property to do not bother at all with the underlying
    server requirements.

    That is, to serve the SLURP defined API, simply define a {!SERVER}
    which must have an event for each method (GET, POST and so on...).
    If you don't want to serve some method operations, for example, simply
    define the corresponding event to [Lwt_react.E.never].

    Once the server interface defined, one must call the {!make} function
    to create a response event that the server can use to forward it to
    the client.

    This event based interface allows the programmer to tune the
    API response by thread, by method or whatever is needed.
*)

(** Server interface. *)
module type SERVER = sig

  (** Event for [GET <URL>] calls. *)
  val get : string event

  (** Event for [POST <URL>] calls with a body, if any. *)
  val post : (string * string) event
end

(** Type abbreviation for {!SERVER}. *)
type server = (module SERVER)


(** [make server] returns a response event from [server] interface. *)
val make : server -> string event
