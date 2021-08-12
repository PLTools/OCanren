Camlp5 syntax extensions
========================

A few syntax extensions are used in this project.

For testing we use the one from ``logger-p5`` opam package. It allows to convert OCaml
expression to its string form. For example, it rewrites ``let _ = REPR(1+2)`` to

.. code-block::

   $ camlp5o `ocamlfind query logger`/pa_log.cmo pr_o.cmo a.ml
   let _ = "1 + 2", 1 + 2

For OCanren itself we use syntax extension to simplify writing relational programs

.. code-block::

   $  cat a.ml
   let _ = fresh (x) z
   $  camlp5o _build/camlp5/pa_ocanren.cmo pr_o.cmo a.ml
   let _ = OCanren.Fresh.one (fun x -> delay (fun () -> z))

.. _ocanren-vs-miniKanren:

OCanren vs. miniKanren
----------------------

The correspondence between original miniKanren and OCanren constructs is shown below:

.. list-table::
   :header-rows: 1

   * - miniKanren
     - OCanren
   * - ``#u``
     - success
   * - ``#f``
     - failure
   * - ``((==) a b)``
     - ``(a === b)``
   * - ``((=/=) a b)``
     - ``(a =/= b)``
   * - ``(conde (a b ...) (c d ...) ...)``
     - ``conde [a &&& b &&& ...; c &&& d &&& ...; ...]``
   * - ``(fresh (x y ...) a b ... )``
     - ``fresh (x y ...) a b ...``



In addition, OCanren introduces explicit disjunction (``|||``) and conjunction
(``&&&``) operators for goals.

.. _let-ocanren:

OCanren-specific Camlp5 syntax extension
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

TODO
