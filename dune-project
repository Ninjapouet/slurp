(lang dune 2.4)

(name slurp)

(generate_opam_files true)

(source (github Ninjapouet/slurp))

(bug_reports "https://github.com/Ninjapouet/slurp/issues")

(package
 (name slurp)
 (synopsis "Sinatra Like URL Route Processing")
 (depends
  (fmt (>= 0.8.8))
  (ezjsonm (>= 1.1.0))
  (uri (>= 3.1.0)))
 (tags ("sinatra" "route" "http" "server"))
 (license "Apache 2.0")
 (authors "Julien Blond <julien.blond@gmail.com>")
 (maintainers "Julien Blond <julien.blond@gmail.com>")
)
