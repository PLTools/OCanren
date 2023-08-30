Installation
============

OCanren can be installed using `opam <https://opam.ocaml.org/doc/Install.html>`_  (>= 2.1). First,
install opam itself, initialize it and install right compiler version.

* ``opam init --bare`` if your opam has not been initialized before
* ``opam switch create 4.14.1 --packages=ocaml-variants.4.14.0+options,ocaml-option-flambda`` installes right compiler. (4.14.1 is a custom switch identifier)
* ``eval $(opam env)`` updates an environment. Opam will put the invocation of this command to your ``~/.bashrc``.
* check that ``ocamlc -v`` prints right compiler version

Then, install dependencies and ``OCanren``:


* ``git clone https://github.com/PLTools/OCanren.git``
* ``cd OCanren``
* Install GT dependency (pick one command)
    * ``git clone https://github.com/PLTools/GT.git`` to use GT as a vendored library.
    * ``opam install GT -y`` to install GT from the main opam repository.
* ``opam install . --deps-only --yes``
* ``make``
* ``make tests``

Expected workflow:
  * add new test to try something out,
  * or use our `template repository <https://github.com/Kakadu/OCanren-basic-template>`_.
