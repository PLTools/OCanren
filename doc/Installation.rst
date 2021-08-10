Installation
============

OCanren can be installed using `opam <https://opam.ocaml.org/doc/Install.html>`_ 2.x. Frist,
install opam itself and right compiler version.


* either ``opam init -c 4.10.1+flambda`` for fresh opam installation
* or ``opam switch create 4.10.1+flambda`` to install minimal required version of OCaml compiler
* or your favourite OCaml version
* ``eval $(opam env)``

Then, install dependencies and ``OCanren``:


* ``opam pin add GT https://github.com/JetBrains-Research/GT.git -n -y``
* ``git clone https://github.com/JetBrains-Research/OCanren.git && cd OCanren``
* ``opam install . --deps-only --yes``
* ``make``
* ``make tests``

Expected workflow: add new test to try something out, or use our `template repository <https://github.com/Kakadu/OCanren-basic-template>`_.
