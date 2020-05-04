
let nothing _ = Lwt.return_unit

let space = Str.regexp " +"

let cmd s = match Str.split space s with
  | h :: t -> (h, Array.of_list (h :: t))
  | _ -> assert false

let shell fmt =
  let k s = fun k -> Lwt_process.(with_process_in (cmd s) k) in
  Fmt.kstr k fmt

let daemon fmt =
  let k s =
    let proc = Lwt_process.(open_process_none
                              ~stderr:(`FD_copy Unix.stdout)
                              ~stdout:(`FD_copy Unix.stdout) (cmd s)) in
    fun () -> proc#kill (-9) in
  Fmt.kstr k fmt

let test path res =
  shell "curl http://localhost:8080/%s" path
    (fun pin ->
       match%lwt Lwt_io.read_line pin#stdout with
       | s when s = res ->
         Fmt.pr "%s = %s OK@." path s;
         Lwt.return_unit
       | s ->
         Fmt.pr "%s = %s KO@." path s;
         Lwt.return_unit
       | exception e ->
         Fmt.pr "%s %a@." path Fmt.exn e;
         Lwt.return_unit)

let _ =
  Lwt_main.run begin
    let cohttp = "../bin/slurpd_cohttp.exe" in
    let services = ["sum"; "sum_lwt"; "json"] in
    let services = List.map (fun s -> Filename.concat "services" (s ^ ".cmxs")) services in
    let kill = daemon "%s -s %a -p %i --static %s"
        cohttp
        Fmt.(list ~sep:(unit ",") string) services
        8080
        "data" in
    (* let the server start... *)
    Lwt_unix.sleep 2.;%lwt

    test "sum/2/3" "5";%lwt
    test "sum_lwt/2/3" "5";%lwt
    test "static/hello.txt" "Hello";%lwt
    test "static/other/world.txt" "World";%lwt
    test "env/default"  "{\"foo\":42,\"bar\":\"pouet\"}";%lwt

    Lwt_unix.sleep 1.;%lwt
    kill ();


    Lwt.return_unit
  end
