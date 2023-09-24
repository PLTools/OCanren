*********************
PPX Syntax extensions
*********************

.. _without-syntax-extensions:

Writing OCanren **without** syntax extensions
=============================================

OCanren and original miniKanren consist of many syntax extension.
Here we describe how to write relations in OCanren without extension.

This is how we write relation without syntax extensions. The top of the input is techinal stuff for loading right packages into OCaml toplevel. The commands assumes that OCanren is already installed (meaning via ``opam install OCanren --yes`` or after ``git clone`` and ``make install``). When you will copy-paste the following examples to terminal, omit leading ``$``.

.. code-block::

  $ ocaml -stdin  -impl - <<EOF
  #use "topfind";;
  #require "OCanren";;
  #rectypes;;

  let rec appendo x y xy =
    let open OCanren in
    let open OCanren.Std in
    conde
      [ (x === Std.nil ()) &&& (y === xy)
      ; Fresh.three (fun h tl tmp ->
          (x === h % tl) &&& (appendo tl y tmp) &&& (xy === h % tmp)
        )
      ]

  let answers =
    let open OCanren in
    run q (fun xs -> appendo (Std.list inj [1; 2]) (Std.list inj [3; 4]) xs)
      (fun xs -> xs#reify (Std.List.prj_exn OCanren.prj_exn))
    |> Stream.take

  let () =
    List.iter (fun xs -> print_endline (GT.show GT.list (GT.show GT.int) xs)) answers
  EOF

Now we will use PPX extension to simplify the code. It allows us

* not to think about count of fresh varaibles;
* automatically insert ``&&&`` when creating fresh variables.

.. code-block::

  $ ocaml -stdin  -impl - <<EOF
  #use "topfind";;
  #require "OCanren";;
  #rectypes;;
  #require "OCanren-ppx.ppx_fresh";;

  let rec appendo x y xy =
    let open OCanren in
    let open OCanren.Std in
    conde
      [ fresh () (x === Std.nil()) (y === xy)
      ; fresh (h tl tmp)
          (x === h % tl)
          (appendo tl y tmp)
          (xy === h % tmp)
      ]

  let answers =
    let open OCanren in
    run q (fun xs -> appendo (Std.list inj [1; 2]) (Std.list inj [3; 4]) xs)
      (fun xs -> xs#reify (Std.List.prj_exn OCanren.prj_exn))
    |> Stream.take

  let () =
    List.iter (fun xs -> print_endline (GT.show GT.list (GT.show GT.int) xs)) answers
  EOF

There is also a camlp5 extension for simplifing relations described `here <./camlp5.html>`__.



PPX syntax extensions
=====================

PPX syntax extensions could be used without camlp5.
They are able to provide miniKanren-specific syntax for relations,
generate types for logic and ground representation for user-defined types,
and provide more convenience for testing relations.
Camlp5 extension relies on PPX extension for genereation of types.
PPX extensions are compilable by ``make ppx``


`ppx_repr`
~~~~~~~~~~

An analogue for `logger <https://opam.ocaml.org/packages/logger-p5>`__ library is called ``ppx_repr`` (located at `OCanren-ppx.ppx_repr` package):

.. code-block::

  $ ocaml -stdin -impl - <<EOF
  #use "topfind";;
  #require "OCanren-ppx.ppx_repr";;

  let (repr,x) = REPR(1+2);;
  let () = Printf.printf "The text expression %S compiles to %d\n" repr x;;
  EOF

`ppx_fresh`
~~~~~~~~~~~

An OCanren-specific syntax extension  extension for creating fresh variables.
It provides canonical miniKanren syntax similar to the one from Scheme-based miniKanren.

.. code-block::

  $ ocaml -stdin -dsource -impl - <<EOF
  #use "topfind";;
  #require "OCanren";;
  #require "OCanren-ppx.ppx_repr";;
  #require "OCanren-ppx.ppx_fresh";;
  #rectypes;;
  open OCanren;;
  let one_el xs = fresh (x) (xs === Std.(x % nil()));;
  let answers =
    run q one_el
      (fun xs -> xs#reify (Std.List.reify OCanren.reify))
    |> Stream.take

  let () =
    List.iter
      (fun xs -> print_endline (GT.show Std.List.logic (GT.show OCanren.logic (GT.show GT.int)) xs))
      answers
  EOF

It should print a representation of singleton list of a free variable. For example:

.. code-block::

   [_.11]

.. note::

  Reread by Kakadu on 2022-09-24 was paused here.

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
