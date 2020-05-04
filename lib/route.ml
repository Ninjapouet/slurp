(* let _ =
 *   (\* slurp : sinatra like URL route processing
 *      yarl : yet another route library  *\)
 *   route *)

(* let _ =
 *   let api = OpenAPI.make "3.0.3"
 *       ~info:(OpenAPI.Info.make "Mon document"
 *                ~description:"C'est un beau document"
 *                ~version:"1.2")
 *       ~paths:[
 *         "/cool", OpenAPI.Path.make ()
 *           ~description:"Cool operation"
 *           ~get:(OpenAPI.Operation.make ()
 *                   ~operationId:"cool"
 *                   ~description:"Yeah"
 *                   ~responses:[]);
 *       ] in
 *   Fmt.pr "%a@." OpenAPI.pp api *)

type json = Ezjsonm.value

type 'a lwt = Lwt : 'a Lwt.t -> 'a lwt
let wrap a = Lwt a


module Type = struct

  type 'a t =
    | Unit : unit t
    | Int : int t
    | String : string -> string t (* mime payload if needed *)
    | JSON : json -> json t
    | HTML : string t
    | Lwt : 'a t -> 'a lwt t

  let rec parse : type a. a t -> string -> a = fun t s ->
    match t with
    | Unit -> ()
    | Int -> int_of_string s
    | String _ -> s
    | JSON _ -> (* don't validate for now *)
      Ezjsonm.value_from_string s
    | HTML -> s
    | Lwt a -> wrap @@ Lwt.return @@ parse a s

  let pp : type a. a t Fmt.t = fun ppf t ->
    match t with
    | Unit -> Fmt.string ppf "unit"
    | Int -> Fmt.string ppf "int"
    | String _ -> Fmt.string ppf "string"
    | JSON _ -> Fmt.string ppf "JSON"
    | HTML -> Fmt.string ppf "HTML"
    | Lwt _ -> Fmt.string ppf "<lwt>"
  [@@warning "-32"]

  let pp_value' : type a. bool -> a t -> a Fmt.t = fun b t -> match t with
    | Unit -> fun _ -> ignore
    | Int -> Fmt.int
    | String _ -> Fmt.string
    | JSON _ -> fun ppf json ->
      Fmt.pf ppf "%s" @@ Ezjsonm.value_to_string ~minify:b json
    | HTML -> Fmt.string
    | Lwt _ -> fun ppf _ -> Fmt.string ppf "<lwt>"

  let pp_value ppf = pp_value' true ppf
  let pp_value_hum ppf = pp_value' false ppf [@@warning "-32"]

  let rec mime_of_type : type a. a t -> string = function
    | Unit -> "text/plain"
    | Int -> "text/plain"
    | String mime -> mime
    | JSON _ -> "application/json"
    | HTML -> "text/html"
    | Lwt a -> mime_of_type a

  let unit = Unit
  let int = Int
  let string mime = String mime
  let json schema = JSON schema
  let html = HTML
end

type 'a spec = {
  typ : 'a Type.t;
  name : string [@main];
  description : string [@default ""];
}[@@deriving make]

let unit = make_spec ~typ:Type.unit ""
let int = make_spec ~typ:Type.int
let string ?(mime = "text/plain") = make_spec ~typ:Type.(string mime)
let json schema = make_spec ~typ:Type.(json schema)
let html = make_spec ~typ:Type.html
let lwt : 'a spec -> 'a lwt spec = fun a -> {
    typ = Type.Lwt a.typ;
    name = a.name;
    description = a.description;
  }

type 'a body = 'a spec
type 'a response = 'a spec
type 'a parameter = 'a spec

type (_, _, _) path =
  | Path : string * ('a, 'b, 'c) path -> ('a, 'b, 'c) path
  | Parameter : 'a parameter * ('b, 'c, 'd) path -> ('a -> 'b, 'c, 'd) path
  | Splat : ('a, 'b, 'c) path -> ('a, 'b, 'c) path
  | Fullsplat : ('a, 'b, 'c) path -> ('a, 'b, 'c) path
  | Query : ('a, 'b, 'c) query -> ('a, 'b, 'c) path

and (_, _, _) query =
  | Bind : 'a parameter * ('b, 'c, 'd) query -> ('a -> 'b, 'c, 'd) query
  | Final : 'a body * 'b response -> ('a -> 'b, 'a, 'b) query

let rec pp_path : type a b c. (a, b, c) path Fmt.t = fun ppf p ->
  match p with
  | Path (s, p') -> Fmt.pf ppf "/%s%a" s pp_path p'
  | Parameter (a, p') -> Fmt.pf ppf "/:%s%a" a.name pp_path p'
  | Splat p' -> Fmt.pf ppf "/*/%a" pp_path p'
  | Fullsplat p' -> Fmt.pf ppf "/**/%a" pp_path p'
  | Query q -> Fmt.pf ppf "/?%a" (pp_query true) q

and pp_query : type a b c. bool -> (a, b, c) query Fmt.t = fun b ppf q ->
  match q with
  | Bind (a, q') ->
    if not b then Fmt.pf ppf "&";
    Fmt.pf ppf "%s%a" a.name (pp_query false) q'
  | Final _ ->
    ()


module Path = struct

  let rec pp_path : type a b c. (a, b, c) path Fmt.t = fun ppf p ->
    match p with
    | Path (s, p') -> Fmt.pf ppf "/%s%a" s pp_path p'
    | Parameter (a, p') -> Fmt.pf ppf "/:%s%a" a.name pp_path p'
    | Splat p' -> Fmt.pf ppf "/*/%a" pp_path p'
    | Fullsplat p' -> Fmt.pf ppf "/**/%a" pp_path p'
    | Query q -> Fmt.pf ppf "/?%a" (pp_query true) q

  and pp_query : type a b c. bool -> (a, b, c) query Fmt.t = fun b ppf q ->
    match q with
    | Bind (a, q') ->
      if not b then Fmt.pf ppf "&";
      Fmt.pf ppf "%s%a" a.name (pp_query false) q'
    | Final _ ->
      ()

  let path s p = Path (s, p)
  let parameter param p = Parameter (param, p)
  let splat p = Splat p
  let fullsplat p = Fullsplat p
  let query q = Query q
  let bind spec q = Bind (spec, q)
  let final body response = Final (body, response)

  let rec in_out : type a b c. (a, b, c) path -> b body * c response = function
    | Path (_, p') -> in_out p'
    | Parameter (_, p') -> in_out p'
    | Splat p' -> in_out p'
    | Fullsplat p' -> in_out p'
    | Query q -> in_out_query q

  and in_out_query : type a b c. (a, b, c) query -> b body * c response = function
    | Bind (_, q') -> in_out_query q'
    | Final (b, r) -> b, r

  let ( / ) : (('a, 'b, 'c) path -> ('d, 'b, 'c) path) -> string -> ('a, 'b, 'c) path -> ('d, 'b, 'c) path =
    fun a b -> fun c -> a (path b c)

  let ( /*/ ) : (('a, 'b, 'c) path -> ('d, 'b, 'c) path) -> string -> ('a, 'b, 'c) path -> ('d, 'b, 'c) path =
    fun a b -> fun c -> a (splat (path b c))

  let ( /**/ ) : (('a, 'b, 'c) path -> ('d, 'b, 'c) path) -> string -> ('a, 'b, 'c) path -> ('d, 'b, 'c) path =
    fun a b -> fun c -> a (fullsplat (path b c))

  let ( /: ) : (('a -> 'b, 'c, 'd) path -> ('e, 'c, 'd) path) -> 'a parameter -> ('b, 'c, 'd) path -> ('e, 'c, 'd) path =
    fun a b -> fun c -> a (parameter b c)

  let ( /*/: ) : (('a -> 'b, 'c, 'd) path -> ('e, 'c, 'd) path) -> 'a parameter -> ('b, 'c, 'd) path -> ('e, 'c, 'd) path =
    fun a b -> fun c -> a (splat (parameter b c))

  let ( /**/: ) : (('a -> 'b, 'c, 'd) path -> ('e, 'c, 'd) path) -> 'a parameter -> ('b, 'c, 'd) path -> ('e, 'c, 'd) path =
    fun a b -> fun c -> a (fullsplat (parameter b c))

  let ( /? ) : (('a, 'b, 'c) path -> ('d, 'b, 'c) path) -> 'e -> (('a, 'b, 'c) query -> ('d, 'b, 'c) path) * 'e =
    fun a b -> (fun q -> a (query q)), b

  let ( /*/? ) : (('a, 'b, 'c) path -> ('d, 'b, 'c) path) -> 'e -> (('a, 'b, 'c) query -> ('d, 'b, 'c) path) * 'e =
    fun a b -> (fun q -> a (splat (query q))), b

  let ( /**/? ) : (('a, 'b, 'c) path -> ('d, 'b, 'c) path) -> 'e -> (('a, 'b, 'c) query -> ('d, 'b, 'c) path) * 'e =
    fun a b -> (fun q -> a (fullsplat (query q))), b

  let ( + ) : ((_ -> _) * _ parameter) -> 'a parameter -> ((_ -> _) * 'a parameter) =
    fun (k, a) b -> (fun q -> k (bind a q)), b

  let ( --> ) : ((('a, 'b, 'c) query -> ('d, 'b, 'c) path) * 'b body) -> 'c response -> ('d, 'b, 'c) path =
    fun (k, b) r -> k (final b r)

end

type meth = [`GET|`POST]

module Route = struct

  type ('a, 'b, 'c) t = {
    meth : meth;
    path : ('a, 'b, 'c) path;
    body : 'b body;
    response : 'c response;
    func : 'a [@main];
    id : string;
    description : string [@default ""];
  }[@@deriving make]

  let make ~meth ~path ~id ?description func =
    let body, response = Path.in_out path in
    make ~meth ~path ~body ~response ~id ?description func
end
open Route

type ('a, 'b, 'c) route = ('a, 'b, 'c) Route.t

let get : path:('a, unit, 'b) path -> id:string-> ?description:string -> 'a -> ('a, unit, 'b) route =
  fun ~path ~id ?description f -> make ~meth:`GET ~path ~id ?description f

let post ~path ~id ?description f =
  make ~meth:`POST ~path ~id ?description f

module M = Map.Make(String)

type table = string list M.t

let sep = Str.regexp "/+"
let cut : string -> (string list * table) = fun s ->
  let uri = Uri.of_string s in
  let path = Uri.path uri in
  let query = Uri.query uri in
  let split_path = Str.split sep path in
  let table = List.fold_left (fun t (k, vals) -> M.add k vals t) M.empty query in
  split_path, table

(* As apply directly try to resove the url-path mapping, we need
   to delay this resolution until the whole patch is matched or
   the corresponding function may be partially called if the path partially
   matches the url. We do not want this, so the computations are
   delayed until we get to the final part. *)
type 'a delayed =
  | Delay : 'a -> 'a delayed
  | App : ('a -> 'b) delayed * 'a -> 'b delayed

let rec resolve : type a. a delayed -> a = function
  | Delay a -> a
  | App (f, a) -> resolve f a

let rec apply : type a b c. string list -> table -> (a, b, c) path -> a delayed -> b -> c =
  fun l t p f ->
  match l, p with
  | [], Query q ->
    apply_query t q f
  | a :: b, Path (s, p') when a = s ->
    apply b t p' f
  | a :: b, Parameter (spec, p') ->
    (match Type.parse spec.typ a with
     | v -> apply b t p' (App (f,  v))
     | exception _ -> invalid_arg "apply")
  | _ :: b, Splat p' ->
    apply b t p' f
  | _ :: b, Fullsplat p' ->
    (match apply l t p' f with
     | f' -> f'
     | exception _ -> apply b t p f)
  | _ -> invalid_arg "apply"

and apply_query : type a b c. table -> (a, b, c) query -> a delayed -> b -> c =
  fun t q f ->
  match q with
  | Bind (spec, q') ->
    (match M.find spec.name t with
     | s :: _ ->
       (match Type.parse spec.typ s with
        | v -> apply_query t q' (App (f, v))
        | exception _ -> invalid_arg "apply_query")
     | _ -> invalid_arg "apply_query"
     | exception Not_found -> invalid_arg "apply_query")
  | Final (_, _) -> resolve f

(* The strong typing we use here makes the route sorting difficult
   but we can assume that the most effectful part of the path
   is its static prefix (all first Path items) which are mostly
   composed by 3-4 elements or more. Sorting path using this
   prefix will likely decrease the lookup complexity from
   O(n * l) to something near O(log2(n) + p * m) where l is
   the mean length of paths, p is
   the mean number of common prefixed operations and m is
   the mean length of paths minus their prefix. For example,
   If we take the Kubernetes API extracted in Kubecaml, we are
   something like l = 4, p = 4 and m = 1 so for
   an API with n = 100 entries, we can expect a lookup
   result in log2(100) + 4 * 1 ~ 10 comparisons against a
   linear lookup of 100 * 5 = 500 in worst case. In best
   cases where the programmer would enter the most used
   paths in the first 10 entries, we have 10 * 5 = 50
   comparisons with a mean of 25.
   For toy REST APIs with at best 5 entries, we have
   a sorted lookup at ~7 comparisons for again 25 in the
   linear lookup.  Again, sorting
   with the path prefix gives better results whatever the
   number of entries is. Only 1 entry API would give the same
   results.
*)

type reg = R : {
    route : ('a, 'b, 'c) route;
    path : ('a, 'b, 'c) path; (* path - prefix *)
  } -> reg

let pp_reg : reg Fmt.t = fun ppf (R r) ->
  Fmt.pf ppf "%a" Path.pp_path r.path

let rec prefix : type a b c. (a, b, c) path -> string list * (a, b, c) path = function
  | Path (s, p') ->
    let l, p'' = prefix p' in
    s :: l, p''
  | p -> [], p

module Q = struct
  type 'a t = {mutable first : 'a cell option; mutable last : 'a cell option}
  and 'a cell = {content : 'a; mutable next : 'a cell option}

  let create () = {first = None; last = None}

  let push q a = match q.last with
    | None ->
      let elt = {content = a; next = None} in
      q.first <- Some elt;
      q.last <- Some elt
    | Some cell ->
      let elt = {content = a; next = None} in
      cell.next <- Some elt;
      q.last <- Some elt

  let rec iter : ('a -> unit) -> 'a t -> unit = fun f q ->
    match q.first with
    | None -> ()
    | Some cell -> iter_cell f cell

  and iter_cell f cell =
    f cell.content;
    match cell.next with
    | None -> ()
    | Some cell' -> iter_cell f cell'

  let pp pp_content ppf q =
    Fmt.pf ppf "{@[";
    iter (fun a -> Fmt.pf ppf "%a;@;" pp_content a) q;
    Fmt.pf ppf "@]}"

  let rec lookup : 'a t -> ('a -> 'b Lwt.t) -> 'b Lwt.t = fun q f ->
    match q.first with
    | None -> Lwt.fail Not_found
    | Some c -> lookup_cell f c

  and lookup_cell f c =
    match%lwt f c.content with
    | v -> Lwt.return v
    | exception _ ->
      (match c.next with
       | None -> Lwt.fail Not_found
       | Some c' -> lookup_cell f c')
end

type ops = {
  get : reg Q.t [@default Q.create ()];
  post : reg Q.t [@default Q.create ()];
}[@@deriving make]

let pp_ops : ops Fmt.t = fun ppf ops ->
  Fmt.pf ppf "%a"
    Fmt.(braces (pair (Q.pp pp_reg) ~sep:(unit ";@;") (Q.pp pp_reg)))
    (ops.get, ops.post)


module R = struct
  include Map.Make(String)

  let pp : _ -> _ Fmt.t = fun p ppf r ->
    Fmt.pf ppf "%a"
      Fmt.(braces (list ~sep:(unit ";@;") (pair string ~sep:(unit " : ") p)))
      (bindings r)
end

(* In the registry we can add the namespaces at the
   same level of operations for the OpenAPI states that
   concrete paths are matched first against path templates. Here,
   we follow that semantic and operations will be searched first
   in namespaces then in ops. *)
type registry = {
  mutable ns : registry R.t [@default R.empty];
  mutable ops : ops [@default make_ops ()];
}[@@deriving make]

let rec pp_registry : registry Fmt.t = fun ppf r ->
  Fmt.pf ppf "%a"
    Fmt.(braces (pair (R.pp pp_registry) ~sep:(unit ";@;") pp_ops)) (r.ns, r.ops)
[@@warning "-32"]

let registry = make_registry ()

let rec lookup_registry : registry -> string list -> registry = fun reg l ->
  match l with
  | [] -> reg
  | h :: t ->
    let reg' = match R.find h reg.ns with
      | reg' -> reg'
      | exception Not_found ->
        let r' = make_registry () in
        reg.ns <- R.add h r' reg.ns;
        r' in
    lookup_registry reg' t

let register : ('a, 'b, 'c) route -> unit = fun r ->
  let pre, path = prefix r.path in
  let reg = lookup_registry registry pre in
  let q = match r.meth with
    | `GET -> reg.ops.get
    | `POST -> reg.ops.post in
  Q.push q (R {route = r; path})

let get ~path ~id ?description f =
  register @@ get ~path ~id ?description f

let post ~path ~id ?description f =
  register @@ post ~path ~id ?description f

let select : meth -> registry -> reg Q.t = fun m r ->
  match m with
  | `GET -> r.ops.get
  | `POST -> r.ops.post

let rec lookup : registry -> meth -> string list -> string list * reg Q.t = fun r m l ->
  (* Fmt.pr "lookup(%a, %a)@."
   *   pp_registry r
   *   Fmt.(brackets (list ~sep:(unit ";@;") string)) l; *)
  match l with
  | [] -> l, select m r
  | h :: t ->
    (match R.find h r.ns with
     | r' -> lookup r' m t
     | exception Not_found ->
       l, select m r)


type data = [`Data of string * string]

let ( let* ) m f = match m with
  | v -> f v
  | exception e -> Lwt.fail e

let ( let+ ) m f = match%lwt m with
  | v -> f v
  | exception e -> Lwt.fail e


let eval : meth -> string -> string -> data Lwt.t = fun meth uri body ->
  (* Fmt.pr "ressource = %s@\nregistry = %a@." uri pp_registry registry; *)
  let l, t = cut uri in
  let* l, q = lookup registry meth l in
  let+ data, mime = Q.lookup q (fun (R {route; path}) ->
      let* f = apply l t path (Delay route.func) in
      let* b = Type.parse route.body.typ body in
      match route.response.typ with
       | Lwt a ->
         let (Lwt th) = f b in
         let%lwt res = th in
         let str = Fmt.str "%a" (Type.pp_value a) res in
         Lwt.return (str, Type.mime_of_type a)
       | a ->
         let res = f b in
         let str = Fmt.str "%a" (Type.pp_value a) res in
         Lwt.return (str, Type.mime_of_type a)) in
  Lwt.return @@ `Data (data, mime)
