*****************
Syntax extensions
*****************

.. _without-syntax-extensions:

Writing OCanren **without** syntax extensions
=============================================

OCanren and original miniKanren consist of many syntax extension. There we describe how to write relation in OCanren without them, to make values of the obvious.

This is how we write relation without syntax extensions. The top of the input is techinal stuff for loading right packages into OCaml toplevel.

.. code-block::

   $ ocaml -stdin  -impl - <<EOF
   #use "topfind";;
   #require "OCanren";;
   #rectypes;;
   open OCanren
   open OCanren.Std

   let rec appendo x y xy =
      conde [
         (x === Std.nil()) &&& (y === xy);
         Fresh.three (fun h tl tmp -> (x === h % tl) &&& (appendo tl y tmp) &&& (xy === h % tmp))
      ]
   EOF

This is how we use PPX exntesion to simplify code. It allows us

* not to think about count of fresh varaibles
* automatically insert ``&&&`` when creating fresh variables

.. code-block::

   $ ocaml -stdin  -impl - <<EOF                                                                                4.13.1+flambda
   #use "topfind";;
   #require "OCanren";;
   #rectypes;;
   open OCanren
   open OCanren.Std

   #require "OCanren-ppx.ppx_fresh";;
   let rec appendo x y xy =
         conde [
            fresh () (x === Std.nil()) (y === xy);
            fresh (h tl tmp) (x === h % tl) (appendo tl y tmp) (xy === h % tmp)
         ]
   EOF

There is also a camlp5 extension for simplifing relations described `here <./camlp5.html>`__.



PPX syntax extensions
=====================

PPX syntax extensions are not related to camlp5 and should be used, for example,
if you want decent IDE support. Main extensions are compilable by ``make ppx``


`ppx_repr`
~~~~~~~~~~

An analogue for `logger <https://opam.ocaml.org/packages/logger-p5>`__ library is called ``ppx_repr`` (located at `OCanren-ppx.ppx_repr` package):

.. code-block::

   $ cat regression_ppx/test002repr.ml
   let _ = REPR(1+2)
   $ ./pp_repr.native regression_ppx/test002repr.ml
   let _ = ("1 + 2", (1 + 2))
   $ ./pp_repr.native -print-transformations
   repr

`ppx_fresh`
~~~~~~~~~~~

An OCanren-specific syntax extension  extension for creating fresh variables. It provides canonical miniKanren syntax from the Scheme

.. code-block::

   $ echo 'let _ = fresh (x) z' | dune exec ppx/pp_fresh.exe --  -impl -
   let _ = Fresh.one (fun x -> delay (fun () -> z))
   $ dune exec ppx/pp_fresh.exe --  -print-transformations
   pa_minikanren

`ppx_distrib`
~~~~~~~~~~~~~

This extension is used to generate smart constructors and reifier from definition of our type. It optionally allows to decorate type definitions with deriving attributes which could be expanded later.

Below we use extension point with two type definitions. First one is nonrecursive fully abstract type. The extension with generate monadic fmap called `fmapt` for it. The second one is a specialization of previous type definition for our needs. it is uses to generate types for `ground`, `logic`, and `injected` values; reifier `reify` and exceptionful projection `prj_exn` from `injected` to `logic`/`ground` values; and smart constructor for creating values of `injected` type.

.. code-block::

   ✗ dune exec ppx/pp_distrib.exe --  -impl - <<EOF
   heredoc>[%%distrib
   type nonrec 'a t =
      | Z
      | S of 'a
   [@@deriving gt ~options:{ gmap; show }]

   type ground = ground t]
   heredoc> EOF
   include
      struct
         type nonrec 'a t =
            | Z
            | S of 'a [@@deriving gt ~options:{ gmap; show }]
         type ground = ground t[@@deriving gt ~options:{ gmap; show }]
         type logic = logic t OCanren.logic[@@deriving gt ~options:{ gmap; show }]
         type injected = injected t OCanren.ilogic
         let fmapt a subj__002_ =
            let open Env.Monad in
            ((Env.Monad.return (GT.gmap t)) <*> a) <*> subj__002_
         let (prj_exn : (_, ground t) Reifier.t) =
            let open Env.Monad in
            let open Env.Monad.Syntax in
               Reifier.fix (fun self -> OCanren.prj_exn <..> (chain (fmapt self)))
         let (reify : (_, logic t OCanren.logic) Reifier.t) =
            let open Env.Monad in
            let open Env.Monad.Syntax in
               Reifier.fix
                  (fun self ->
                     OCanren.reify <..>
                     (chain (Reifier.zed (Reifier.rework ~fv:(fmapt self)))))
         let z () = OCanren.inji Z
         let s _x__001_ = OCanren.inji (S _x__001_)
      end

`ppx_deriving_reify`
~~~~~~~~~~~~~~~~~~~~

Simplifies inline generation of reifiers for already known types.

.. code-block::

   $ dune exec ppx/pp_deriving_reify.exe --  -impl - <<EOF
   let _ = [%reify: GT.int GT.list]
   EOF
   let _ = Std.List.reify OCanren.reify
