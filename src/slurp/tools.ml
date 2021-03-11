
let re = Str.regexp "/+"

let rec paths_of_dir : _ -> string -> (string * _) list Lwt.t =
  fun path dir ->
  let files = Lwt_unix.files_of_directory dir in
  paths_of_files path dir files

and paths_of_files : _ -> string -> string Lwt_stream.t -> _ =
  fun p dir stream ->
  match%lwt Lwt_stream.get stream with
  | None -> Lwt.return []
  | Some ("." | "..") -> paths_of_files p dir stream
  | Some f ->
    let path = Filename.concat dir f in
    (match Sys.is_directory path with
     | true ->
       let%lwt f_paths = paths_of_dir Route.Path.(p / f) path in
       let%lwt nexts = paths_of_files p dir stream in
       Lwt.return @@ f_paths @ nexts
     | false ->
       let%lwt nexts = paths_of_files p dir stream in
       Lwt.return @@ (path, Route.Path.(p / f)) :: nexts)

let static_content path =
  let%lwt ic = Lwt_io.(open_file ~mode:input) path in
  let%lwt content = Lwt_io.read ic in
  Lwt_io.close ic;%lwt
  Lwt.return content

let static ?(prefix = "static") paths =
  let prefix_path = Route.Path.path prefix in
  Lwt_list.iter_s (fun dir ->
      let%lwt paths = paths_of_dir prefix_path dir in
      List.iter (fun (real_path, route_path) ->
          let open Route in
          let mime = Magic_mime.lookup real_path in
          let path = Path.(
              route_path /? unit --> lwt (string ~mime "result")) in
          (* Fmt.pr "%s: %a@." real_path Route.pp_path path; *)
          Route.(get ~id:"" ~path
                   (fun () -> wrap @@ static_content real_path))) paths;
      Lwt.return_unit) paths
