from ocaml/opam2:alpine

# installing OS stuff
user root
run apk add m4

# update opam
user opam
run git pull
run opam repository add aux -k git https://github.com/Ninjapouet/opam-repository 2
run opam update
run opam upgrade

# prepare sources
workdir /home/opam
copy --chown=opam . pkg
workdir pkg
run git clean -dxf

# install
run opam install . -y -t -d
