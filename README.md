# SLURP

Sinatra Like URL Route Processing (SLURP) is another route framework to define REST API à la Sinatra. SLURP is strongly typed  and modular. It is not bound to a server definition so that anyone can use it in existing projects and new ones with custom servers (Cohttp, H2, Apache, Nginx or whatever).

# Installation

SLURP isn't published to opam yet. Simply clone the repository and install it manually:
    
    git clone https://github.com/Ninjapouet/slurp.git
    cd slurp
    opam install .

# Documentation

SLURP documentation isn't published yet but is available within a repository:

    dune build @doc

and simply open \_build/default/\_doc/\_html/index.html with your favorite browser.

# Related Work
Currently, there are three related projects that gives the same route based API definition: [Opium](https://github.com/rgrinberg/opium), [resto](https://gitlab.com/nomadic-labs/resto/).
