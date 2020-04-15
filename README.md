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
Currently, there are three related projects on opam that gives the same route based API definition: [Opium](https://github.com/rgrinberg/opium), [resto](https://gitlab.com/nomadic-labs/resto/) and [OWebl](https://github.com/eatonphil/owebl/). To be short, the table below summarize the differences between those projects and SLURP (as far I as understand them).

| Feature          | Opium   | Resto       | OWebl   | SLURP      |
| ---              | :---:   | :---:       | :---:   | :---:      |
| Strongly typed   |         |   X         |         |  X         |
| \*\|\*\* support |   X     |             | ?       |  X(1)      |
| Query support    |         |   X         | X       |  X         |
| Body support     | Manual  |   X         | X       |  X         |
| Stream support   | Manual  |   X         |  ?      |            |
| Library          |         |   X(2)      |         |  X(2)      |
| Export           |         | Custom JSON |         | OpenAPI(3) |
| Complexity       | O(n)    |  ?          |  ?      | O(log2(n)) |

- (1) I don't know if the semantic given to these operators is the common one, I'll check that someday...
- (2) But server implementations (are|will be) available.
- (3) As planned with [OpenAPI](https://github.com/Ninjapouet/openapi) project which will give client code generation support.

As you can see, [resto](https://gitlab.com/nomadic-labs/resto/) propose something close to SLURP but with some design issues I feel uncomfortable with. The API is a library but clearly designed to be used with [Cohttp](https://github.com/mirage/ocaml-cohttp) and for [Tezos](https://gitlab.com/tezos/tezos) which are great projects but I personnaly need something more "raw" and standard for companies I work for. This standard support is mandatory for the API description can lead to code generation on client side with other languages (Python, Java or whatever) with *existing* code generators.

I really liked testing [Opium](https://github.com/rgrinberg/opium) for its raw simplicity suited well to some of my need but the tool isn't ready for industrial use because of its lack of modularity (in my point of view). I didn't tried much [OWebl](https://github.com/eatonphil/owebl/) for its author claims himself to not use it in real project :wink:.
