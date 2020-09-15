Launch the server.
  $ ./../bin/slurpd_cohttp.exe -s services/sum.cmxs,services/sum_lwt.cmxs,services/json.cmxs -p 12345 --static data > _log 2>&1 &
  $ echo $! > _pid
Wait a bit to let the server start...
  $ sleep 1
  $ cat _log

Test sum:
  $ curl -s --show-error http://localhost:12345/sum/2/3
  5

Test sum_lwt:
  $ curl -s --show-error http://localhost:12345/sum_lwt/2/3
  5

Test static file:
  $ curl -s --show-error http://localhost:12345/static/hello.txt
  Hello

Test static file with directory:
  $ curl -s --show-error http://localhost:12345/static/other/world.txt
  World

Test json data:
  $ curl -s --show-error http://localhost:12345/env/default
  {"foo":42,"bar":"pouet"}

Kill the server if we can. Some opam CI environments don't have kill command
but that's not important since such environments are VM/docker/sandbox killed
by the way.
  $ test -n "$(which kill)" && (cat _pid | xargs kill -9) || true
