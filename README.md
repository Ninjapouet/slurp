# SLURP

Sinatra Like URL Route Processing (SLURP) is another route framework to
define REST API à la Sinatra. SLURP is strongly typed and modular. It is
not bound to a server definition so that anyone can use it in existing
projects and new ones with custom servers (Cohttp, H2, Apache, Nginx or
whatever).

# Installation

SLURP is intented to be used with opam with:

	opam install slurp

Server support comes with other package named slurp-xxx where xxx is the
server name. Currently ony [Cohttp](https://github.com/mirage/ocaml-cohttp)
is supported but other server will be added on demand (and with time). To
install the [Cohttp](https://github.com/mirage/ocaml-cohttp) SLURP
server stuff, simply use:

	opam install slurp-cohttp

# Documentation

SLURP documentation can be found [here](ninjapouet.github.io/slurp/index.html).

# Related Work

At the time of writing, there are three related projects on opam that gives the same
route based API definition: [Opium](https://github.com/rgrinberg/opium),
[resto](https://gitlab.com/nomadic-labs/resto/) and
[OWebl](https://github.com/eatonphil/owebl/). To be short, the table below summarize
the differences between those projects and SLURP (as far I as understand them).

| Feature          | Opium   | Resto       | OWebl   | SLURP      |
| ---              | :---:   | :---:       | :---:   | :---:      |
| Strongly typed   |         |   X         |         |  X         |
| \*\|\*\* support |   X     |             | ?       |  X(1)      |
| Query support    |         |   X         | X       |  X(2)      |
| Body support     | Manual  |   X         | X       |  X         |
| Stream support   | Manual  |   X         |  ?      |            |
| Library          |         |   X         |         |  X         |
| Export           |         | Custom JSON |         | OpenAPI(3) |

- (1) I don't know if the semantic given to these operators is the common one,
  I'll check that someday...
- (2) The query part is operationnal but not with the intended usage. It will
  be fixed for a near version.
- (3) As planned with [OpenAPI](https://github.com/Ninjapouet/openapi) project
  which will give client code generation support.

As you can see, [resto](https://gitlab.com/nomadic-labs/resto/) propose something
close to SLURP but with some design issues I feel uncomfortable with. The API is a
library but clearly designed to be used with [Cohttp](https://github.com/mirage/ocaml-cohttp)
and for [Tezos](https://gitlab.com/tezos/tezos) which are great projects but I
personnaly need something more "raw" and standard for companies I work for. This
standard support is mandatory for the API description can lead to code generation on
client side with other languages (Python, Java or whatever) with *existing* code
generators.

I really liked testing [Opium](https://github.com/rgrinberg/opium) for its raw
simplicity suited well to some of my need but the tool isn't ready for industrial use
because of its lack of modularity (in my point of view). I didn't tried much
[OWebl](https://github.com/eatonphil/owebl/) for its author claims himself to not use it
in real project :wink:.
