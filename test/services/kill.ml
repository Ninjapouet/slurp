(* A special service to kill the server for some CI environment (opam's)
   do not have the kill shell command *)

open Slurp
open Route

let () = get
    ~id:"kill"
    ~path:Path.(path "kill" /? unit --> string "OK")
    (fun () ->
       (* wait a bit to allow answering for curl doesn't like empty replies *)
       Lwt.async (fun () -> Lwt_unix.sleep 1.;%lwt exit 0);
       "OK")
