
Set some test definitions...
  $ export slurp=./../bin/slurpd_cohttp.exe
  $ test () { curl -s --show-error "http://localhost:12345/$1"; }
  $ kill () { test "kill"; sleep 1; }

Launch the server.
  $ ${slurp} -s services/kill.cmxs,services/sum.cmxs,services/sum_lwt.cmxs,services/json.cmxs -p 12345 --static data &
  $ echo $! > _pid
Wait a bit to let the server start...
  $ sleep 1

Test sum:
  $ test sum/2/3
  5

Test sum_lwt:
  $ test sum_lwt/2/3
  5

Test static file:
  $ test static/hello.txt
  Hello

Test static file with directory:
  $ test static/other/world.txt
  World

Test json data:
  $ test env/default
  {"foo":42,"bar":"pouet"}

Kill the server.
  $ kill
  OK


Regression test #17:
  $ ${slurp} -s services/kill.cmxs,nofile.cmxs -p 12345 --static data &
  [error] cannot open nofile.cmxs

  $ kill
  OK
