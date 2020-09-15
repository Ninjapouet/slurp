(lang dune 2.7)
(cram enable)

(name slurp)

(generate_opam_files true)

(version 0.1.0)

(source (github Ninjapouet/slurp))

(bug_reports "https://github.com/Ninjapouet/slurp/issues")

(package
 (name slurp)
 (synopsis "Sinatra Like URL Route Processing")
 (depends
  (ocaml (>= 4.08.1))
  (fmt (>= 0.8.8))
  (ezjsonm (>= 1.1.0))
  (uri (>= 3.1.0))
  (lwt (>= 5.2.0))
  (lwt_ppx (>= 2.0.1))
  (magic-mime (>= 1.1.2))
  (fpath (>= 0.7.3))
  (lwt_react (>= 1.1.3))
 )
 (tags ("sinatra" "route" "http" "server"))
 (license "Apache 2.0")
 (authors "Julien Blond <julien.blond@gmail.com>")
 (maintainers "Julien Blond <julien.blond@gmail.com>")
)

(package
 (name slurp-cohttp)
 (synopsis "Cohttp definition for Slurp")
 (depends
  (slurp (>= 0.1.0))
  (cohttp-lwt-unix (>= 2.5.1))
  (ezcmdliner (>= 0.1.1))
)
 (tags ("sinatra" "route" "http" "server" "cohttp"))
 (license "Apache 2.0")
 (authors "Julien Blond <julien.blond@gmail.com>")
 (maintainers "Julien Blond <julien.blond@gmail.com>")
)
