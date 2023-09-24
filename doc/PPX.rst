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

`ppx_tester`
~~~~~~~~~~~~~

There is `OCanren.tester` library which simplifies running and printing results of the query.
The main function is ``run_r`` which takes reifier, pretty-printer, number of expected results, a query
and two numerals to support polyvariadic query. Adding these numerals may be cumbersome, so there is a syntax extension which calculates right numeral from the number of lambda-abstration in the passed relation.

You can use the ``tester`` library via

.. code-block::

  $ ocaml -stdin -dsource -impl - <<EOF
  #use "topfind";;
  #require "OCanren";;
  #require "OCanren.tester";;
  #require "OCanren-ppx.ppx_repr";;
  #require "OCanren-ppx.ppx_tester";;
  #require "OCanren-ppx.ppx_fresh";;
  #rectypes;;
  open OCanren;;
  open Tester;;
  let _ =
    (* without ppx_tester *)
    run_r OCanren.reify (GT.show OCanren.logic (GT.show GT.int)) 1
      q Tester.qh
      ("<string repr of goal>", (fun q -> q === (inj 1)));;

  let _ =
    (* with ppx_tester *)
    [%tester
      run_r OCanren.reify (GT.show OCanren.logic (GT.show GT.int)) 1
        (fun q -> q === (inj 1))];;

  EOF

It will print something like

.. code-block::

  <string repr of goal>, 1 answer {
  q=1;
  }
  fun q -> q === (inj 1), 1 answer {
  q=1;
  }

`ppx_distrib`
~~~~~~~~~~~~~

This extension is used to generate smart constructors and reifier from definition of our type. It optionally allows to decorate type definitions with deriving attributes which could be expanded later.

Below we use extension point with two type definitions. First one is nonrecursive fully abstract type. The extension with generate monadic fmap called `fmapt` for it. The second one is a specialization of previous type definition for our needs. it is uses to generate types for `ground`, `logic`, and `injected` values; reifier `reify` and exceptionful projection `prj_exn` from `injected` to `logic`/`ground` values; and smart constructor for creating values of `injected` type.

.. code-block::

    ✗ ocaml -stdin -dsource -impl - <<EOF
    #use "topfind";;
    #require "OCanren";;
    #require "OCanren-ppx.ppx_distrib";;
    #require "GT.ppx_all";;
    #rectypes;;
    [%%distrib
      type nonrec 'a t =
        | Z
        | S of 'a
      [@@deriving gt ~options:{ gmap; show }]

      type ground = ground t];;

    #show_type t;;
    #show_type ground;;
    #show_type logic;;
    #show reify;;
    #show prj_exn;;

    EOF

It will output expanded type and value definitions.

.. code-block::

    type 'a t = Z | S of 'a
    type ground = ground t
    type logic = logic t OCanren.logic
    val reify : (injected, logic) OCanren.Reifier.t
    val prj_exn : (injected, ground) OCanren.Reifier.t


`ppx_deriving_reify`
~~~~~~~~~~~~~~~~~~~~

Simplifies inline generation of reifiers for already known types. With it we can specify a type, and syntax extension will try to build reifier for it. There is a syntax exension in GT, which work similarly.

.. code-block::

    ✗ ocaml -stdin -dsource -impl - <<EOF
    #use "topfind";;
    #require "OCanren";;
    #require "OCanren.tester";;
    #require "OCanren-ppx.ppx_deriving_reify";;
    #require "GT.ppx_all";;
    #rectypes;;
    open OCanren;;
    open Tester;;
    let _ =
      run_r
        [%prj_exn: GT.int OCanren.Std.List.injected]
        ([%show:    GT.int OCanren.Std.List.ground] ())
        1
        q Tester.qh
        ("", (fun q -> q === Std.list inj [1;2;3]));;
    EOF

will output

.. code-block::

    , 1 answer {
    q=[1; 2; 3];
    }
