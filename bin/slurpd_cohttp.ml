
let () = Lwt_main.run (Ezcmdliner.run Slurp_cohttp.Server.command)
