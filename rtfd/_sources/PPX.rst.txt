Syntax extensions
=================

.. _without-syntax-extensions:

Writing OCanren **without** syntax extensions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

TODO


PPX syntax extensions
~~~~~~~~~~~~~~~~~~~~~

PPX syntax extensions are not related to camlp5 and should be used, for example,
if you want decent IDE support. Main extensions are compilable by ``make ppx``

An analogue for logger library is called ``ppx_repr``\ :

.. code-block::

   $ cat regression_ppx/test002repr.ml
   let _ = REPR(1+2)
   $ ./pp_repr.native regression_ppx/test002repr.ml
   let _ = ("1 + 2", (1 + 2))
   $ ./pp_repr.native -print-transformations
   repr

An OCanren-specific syntax extension includes both ``ppx_repr`` and extension for
creating fresh variables

.. code-block::

   $ cat a.ml
   let _ = fresh (x) z
   $ ./pp_ocanren_all.native a.ml
   let _ = OCanren.Fresh.one (fun x -> delay (fun () -> z))
   $ ./pp_ocanren_all.native -print-transformations
   pa_minikanren
   repr

There also syntax extensions for simplifyng developing data type for OCanren
but they are not fully documented.
