
let shell fmt =
  let k s = fun k ->
    (* Fmt.pr "shell: %s@." s; *)
    Lwt_process.(with_process_in (shell s) k) in
  Fmt.kstr k fmt

let test path res =
  shell "curl -s http://localhost:8080/%s" path
    (fun pin ->
       match%lwt Lwt_io.read_line pin#stdout with
       | s when s = res ->
         Fmt.pr "%s OK@." path;
         Lwt.return_unit
       | _ ->
         Fmt.pr "%s KO@." path;
         Lwt.return_unit
       | exception e ->
         Fmt.pr "%s %a@." path Fmt.exn e;
         Lwt.return_unit)

let _ =
  Lwt_main.run begin
    let cohttp = "../cohttp/server.exe" in
    let service = "services/sum.cmxs" in
    let ic = Unix.open_process_args_in cohttp
        [|cohttp; "-s"; service; "-p"; "8080"|]in
    Lwt_unix.sleep 1.;%lwt

    test "sum/2/3" "5";%lwt

    let pid = Unix.process_in_pid ic in
    Unix.kill pid (-9);
    Lwt.return_unit
  end
