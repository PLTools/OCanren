Welcome to OCanren's documentation!
===================================

.. toctree::
   :maxdepth: 1

   Installation
   PPX
   Appendo
   tuto_main/tuto_main
   camlp5
   papers




OCanren is a strongly-typed embedding of `miniKanren <http://minikanren.org>`_ relational
programming language into `OCaml <http://ocaml.org>`_. Nowadays, implementation of
OCanren strongly reminds `faster-miniKanren <https://github.com/michaelballantyne/faster-miniKanren>`_.
Previous implementation was based on
`microKanren <http://webyrd.net/scheme-2013/papers/HemannMuKanren2013.pdf>`_
with `disequality constraints <http://scheme2011.ucombinator.org/papers/Alvis2011.pdf>`_.


OCanren vs. miniKanren
----------------------

The syntax between OCanren and vanilla miniKanren is a little bit different:  :ref:`ocanren-vs-miniKanren`.
The correspondence between original miniKanren and OCanren constructs is shown below:



Injecting and Projecting User-Type Data
---------------------------------------




To make it possible to work with OCanren, user-type data have to be *injected* into
logic domain. In the simplest case (non-parametric, non-recursive) the function

.. code-block:: ocaml

   val inj : 'a -> 'a ilogic

can be used for this purpose:

.. code-block:: ocaml

   inj 1
   inj true
   inj "abc"


The presence of `ilogic` type demonstrates that a logic variables could be placed there.

.. raw:: html

   <!--
   If the type is parametric (but non-recursive), then (as a rule) all its type parameters
   have to be injected as well:

   ```ocaml
   !! (gmap(option) (!!) (Some x))
   ```

   ```ocaml
   !! (gmap(pair) (!!) (!!) (x, y))
   ```

   Here `gmap(type)` is a type-indexed morphism for the type `type`; it can be written
   by hands, or constructed using one of the existing generic programming
   frameworks (the library itself uses [GT](https://github.com/dboulytchev/generic-transformers)).
   -->



If the type is a algebraic type definition, then, as a rule, it has to be
abstracted from itself, and then we can write one smart constructors to build
injected values.
This abstraction allows us to place logic values (i.e. variables) as arbitrary arguments of an algebric value.
Obviously, abstracted type will be isomorphic to the original one.

.. code-block:: ocaml

   type tree = Leaf | Node of tree * tree

is converted into

.. code-block:: ocaml

    type 'self tree = Leaf | Node of 'self * 'self

    let leaf : unit -> _ tree ilogic = fun () -> inj Leaf
    let node : 'a tree -> 'a tree -> a tree ilogic = fun  b c -> inj (Node (b, c))

Using fully abstract type we can construct type of so called ``ground`` trees
(without logic values) and type of ``logic trees`` --
the trees that can contain logic variables inside.

Using this fully abstract type and a few OCanren builtins we can
construct ``reification`` procedure which translates ``'a ilogic`` values into non-logic representations.

.. code-block:: ocaml

    module Tree = struct
      ...
      type ground   = ground   tree
      type logic    = logic    tree Logic.logic
      type injected = injected tree ilogic
    end

The construction of reifier is based on predefined reifiers, fmap-ing of user-defined type and tying recursive know. In practice, construction of reifier manually could require deep knowledge of OCanren internals, so using a syntax extension is recommended.

First of all, we need to get functor action for our type. It could be done manually, or using generic programming approch (the one using GT is the most battle-tested one).

.. code-block:: ocaml

    let fmap f = function
    | Leaf -> Leaf
    | Node (a,b) -> Node (f a, f b)

After that we can construct reifiers into various representations. The most common one, is the `logic` one where all logic values are specifiied explicitly in the type. Another one is the ground reification called `prj_exn`,
which translated value into ground representation, unless there are some logic variables in the input.

.. code-block:: ocaml

    module Tree = struct
      ...
      let (prj_exn : (injected, ground) OCanren.Reifier.t) =
        let open OCanren.Env.Monad in
        OCanren.Reifier.fix (fun self -> OCanren.prj_exn <..> chain (fmap self))

      let (reify : (injected, logic) OCanren.Reifier.t) =
        let open OCanren.Env.Monad in
        OCanren.Reifier.fix (fun self ->
          OCanren.reify
            <..> chain
                    (OCanren.Reifier.zed
                      (OCanren.Reifier.rework ~fv:(fmapt self)) ) )
    end

The reifiers of type `('from, 'to) Reifier.t` reify logic values from type `'from` to the type `'to`. Note how `prj_exn` and `reify` reify trees in the injected representation to the distinct types.

We could use reifiers to query a relational program and reify the resulting values. For example,

.. code-block:: ocaml

    let _: Tree.logic OCanren.Stream.t =
      run q (fun q  -> q === leaf ())
            (fun qs -> qs#reify Tree.reify)


Bool, Nat, List
---------------

There is some built-in support (in the module `OCanren.Std`) for a few basic types --- booleans, natural
numbers in Peano form, logical lists. See corresponding modules.

The following table summarizes the correspondence between some expressions
on regular lists and their OCanren counterparts:

.. list-table::
   :header-rows: 1

   * - Regular lists
     - OCanren (infix, from the `Std` module)
     - OCanren (prefix)
   * - ``[]``
     - ``nil ()``
     - ``Std.nil ()``
   * - ``x::xs``
     - ``x % xs``
     - ``Std.List.cons x y``
   * - ``[x]``
     - ``!< x``
     - ``Std.List.singleton x``
   * - ``[x; y]``
     - ``x %< y```
     - ``Std.List.cons x (Std.List.singleton y)``
   * - ``[x; y; z]``
     - ``x % (y %< z)``
     - ``Std.List.cons x (Std.List.cons y (Std.List.singleton z))``
   * - ``x::y::z::tl``
     - ``x % (y % (z % tl))``
     - ``Std.List.cons x (Std.List.cons y (Std.List.cons z tl))``


Syntax Extensions
-----------------

The `camlp5` based syntax extension adds two constructs, implemented as syntax extensions: ``fresh`` and ``defer``.
The latter is used to eta-expand enclosed goal ("inverse-eta delay").

However, neither of them actually needed. Instead of ``defer (g)`` manual expansion can
be used:

.. code-block:: ocaml

   delay (fun () -> g)

To get rid of ``fresh`` one can use ``OCanren.Fresh`` module, which introduces variadic function
support by means of a few predefined numerals and a successor function. For
example, instead of

.. code-block:: ocaml

   fresh (x y z) g

one can write

.. code-block:: ocaml

   Fresh.three (fun x y z -> g)

or even

.. code-block:: ocaml

   (Fresh.succ Fresh.two) (fun x y z -> g)

Run
---

The top-level primitive in OCanren is ``run``, which can be used in the following
pattern:

.. code-block:: ocaml

  run n (fun q1 q2 ... qn -> g) (fun a1 a2 ... an -> h)

* Here ``n`` stands for *numeral* --- a value, describing the number of arguments;
* ``q1``, ``q2``, ..., ``qn`` --- free logic variables;
* ``a1``, ``a2``, ..., ``an`` --- streams of answers for ``q1``, ``q2``, ..., ``qn`` respectively;
* ``g`` --- a goal we are going to execute;
* ``h`` --- a *handler* (some piece of code, presumable making use of ``a1``, ``a2``, ..., ``an``).

There are a few predefined numerals (``q``, ``qr``, ``qrs``, ``qrst`` etc.) and a
successor function, ``succ``, which can be used to "manufacture" greater
numerals from smaller ones.

Sample
------

Here a complete example of OCanren specification (relational binary search tree).
The original code could be found in https://github.com/PLTools/OCanren/blob/master/samples/tree.ml

.. code-block:: ocaml

  open OCanren

  module Tree = struct
    ocanren type 'a tree = Leaf | Node of 'a * 'a tree * 'a tree

    type inttree = GT.int tree [@@deriving gt ~options:{show}]
    (* A shortcut for "ground" tree we're going to work with in "functional" code *)
    type rtree = Std.Nat.ground tree [@@deriving gt ~options:{show}]

    (* Logic counterpart *)
    type ltree = Std.Nat.logic tree_logic [@@deriving gt ~options:{show}]

    let leaf    () : Std.Nat.injected tree_injected = inj Leaf
    let node a b c : Std.Nat.injected tree_injected = inj @@ Node (a,b,c)

    (* Injection *)
    let rec inj_tree : inttree -> Std.Nat.injected tree_injected = fun tree ->
      inj @@ GT.(gmap tree_fuly Std.nat inj_tree tree)

    (* Projection *)
    let rec prj_tree : rtree -> inttree =
      fun eta -> GT.(gmap tree_fuly) Std.Nat.to_int prj_tree eta

    let reify_tree : (Std.Nat.injected tree_injected, ltree) Reifier.t =
      tree_reify Std.Nat.reify

    let prj_exn_tree : (Std.Nat.injected tree_injected, inttree) Reifier.t =
      let rec tree_to_int x = GT.gmap tree_fuly Std.Nat.to_int (tree_to_int) x in
      Reifier.fmap tree_to_int (tree_prj_exn Std.Nat.prj_exn)
  end

  let () =
    let open Tree in
    (* Demo about full blown reification *)
    let answers: Tree.ltree Stream.t =
      run q (fun q -> q === leaf ())
          (fun qs -> qs#reify Tree.reify_tree)
    in
    assert (Stream.take answers = [Value Leaf]);
    (* reification to ground representation *)
    let answers: Tree.inttree Stream.t =
      run q (fun q -> q === leaf ())
          (fun qs -> qs#reify Tree.prj_exn_tree)
    in
    assert (Stream.take answers = [Leaf])

  (* Relational insert into a search tree *)
  let rec inserto a t' t'' =
    let open Tree in
    conde
      [ (t' === leaf ()) &&& (t'' === node a (leaf ()) (leaf ()))
      ; fresh (x l r l')
          (t' === node x l r)
          (conde [
            (t'' === t') &&& (a === x);
            (t'' === node x l' r ) &&& Std.Nat.(a < x) &&& inserto a l l';
            (t'' === node x l  l') &&& Std.Nat.(a > x) &&& inserto a r l';
          ])
      ]

  (* Top-level wrapper for insertion --- takes and returns non-logic data *)
  let insert : int -> Tree.inttree -> Tree.inttree = fun a t ->
    Stream.hd @@
    run q (fun q  -> inserto (Std.nat a) (Tree.inj_tree t) q)
          (fun qs -> qs#reify Tree.prj_exn_tree)

  (* Top-level wrapper for "inverse" insertion --- returns an integer, which
    has to be inserted to convert t into t' *)
  let uninsert t t' =
    Std.Nat.to_int @@ Stream.hd @@
    run q (fun q  -> inserto q (Tree.inj_tree t) (Tree.inj_tree t'))
          (fun qs -> qs#reify Std.Nat.prj_exn)

  (* Entry point *)
  let _ =
    let open Printf in
    let insert_list xs =
      let f acc x =
        let acc2 = insert x acc in
        printf "Inserting %d into %s makes %s\n%!" x (Tree.show_inttree acc)
          (Tree.show_inttree acc2);
        acc2
      in
      (* The opening of OCanren hides Stdlib.List *)
      Stdlib.List.fold_left  f Leaf xs
    in
    ignore @@ insert_list [1; 2; 3; 4];
    let t  = insert_list [3; 2; 4; 1] in
    let t' = insert 8 t in
    printf "Inverse insert: %d\n" @@ uninsert t t'
