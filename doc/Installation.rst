Installation
============

OCanren can be installed using `opam <https://opam.ocaml.org/doc/Install.html>`_  (>= 2.1). First,
install opam itself, initialize it and install right compiler version.

* ``opam init --bare`` if your opam has not been initialized before
* ``opam switch create 4.14.0 --packages=ocaml-variants.4.14.0+options,ocaml-option-flambda`` installes right compiler. (4.14.0 is a custom switch identifier)
* ``eval $(opam env)`` updates an environment. Opam should write invocation of this command to your ``/.bashrc``
* check that ``ocamlc -v`` prints right compiler version

Then, install dependencies and ``OCanren``:


* Install GT (pick one command)
    * ``opam install GT -y`` to install GT from opam repository.
    * ``opam pin add GT https://github.com/JetBrains-Research/GT.git -n -y`` for fresh GT.
* ``git clone https://github.com/JetBrains-Research/OCanren.git``
* ``cd OCanren``
* ``opam install . --deps-only --yes``
* ``make``
* ``make tests``

Expected workflow:
  * add new test to try something out,
  * or use our `template repository <https://github.com/Kakadu/OCanren-basic-template>`_.
