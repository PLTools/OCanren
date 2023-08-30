[![OCanren][1]][2]
[![License](https://img.shields.io/badge/license-LGPL-blue)](https://github.com/JetBrains-Research/spla/blob/master/LICENSE.md)
[![API docs](https://img.shields.io/badge/API-documentation-yellowgreen)](https://PLTools.github.io/OCanren/api/)


[1]:  https://github.com/PLTools/OCanren/actions/workflows/master.yml/badge.svg
[2]:  https://github.com/PLTools/OCanren/actions

`OCanren` is a strongly-typed embedding of relational programming language [miniKanren](http://minikanren.org)
into [OCaml](http://ocaml.org). Nowadays, the implementation of `OCanren` strongly reminds [faster-miniKanren](https://github.com/michaelballantyne/faster-miniKanren).
Previous implementation was based on [microKanren](http://webyrd.net/scheme-2013/papers/HemannMuKanren2013.pdf)
with [disequality constraints](http://scheme2011.ucombinator.org/papers/Alvis2011.pdf).

See [installation instructions](https://ocanren.readthedocs.io/en/latest/Installation.html#installation),
[API documentation](https://PLTools.github.io/OCanren/api/) for more details, or [ReadTheDocs](https://ocanren.rtfd.io/) for detailed information. (All papers about OCanren are not located in the [separate repo](https://github.com/PLTools/papers).)

Check our [template repository for OCanren projects](https://github.com/Kakadu/OCanren-basic-template) for faster startup!

### Installation

To try out developer version of `OCanren` and `OCanren-ppx` both, use

      opam pin add https://github.com/PLTools/OCanren.git --yes

`OCanren` [is available](https://ocaml.org/packages/search?q=OCanren) in the main OPAM repository, but it's version may be lagging behind.

      opam install OCanren-ppx OCanren --yes
