.. _Digesting the Types:

Digesting the Types
===================

OCanren is designed for typed relational programming. Two points here:
it is typed, and it is relational. We shall now study how to work with
the types. This clears the way so that we can then focus on the
relational part.

We have seen that the OCanren internal representation of a string has a
type of the form ``('a, 'b) injected`` and we have named it an
*injected type*, referring to the injection of data from user level
representation into the internal representation. This type expression
involves several subtleties that are, when combined together, not
apparent. In this lesson we break down such type expressions into their
very components, so that the reader can appreciate the construction of
these internal types and can build his own.

Fully Abstract Types
--------------------

First we need a notion of *abstract type*. OCaml also has a notion of
abstract type which refers to a type constructor whose equation and
representation are hidden from the user and is considered incompatible
with any other type. However, the fully abstract type that we are talking
about here is a different concept, and it comes from the fact that some
recurive types can be defined in the following way.

.. todo::

   Mention where fully abstract types were 1st time introduced.

Say we want to define a polymorphic list type:

.. code:: ocaml

   module MyList = struct
     type ('a, 'b) t = Nil | Cons of 'a * 'b
   end

The type constructor ``MyList.t`` is called an *fully abstract list type* for
it not only abstracts over the list member type by means of the type
parameter ``'a``, but also (and more importantly) abstracts over the
list tail type or in other words over the list type itself by means of
the type parameter ``'b``. We can use the fully abstract list type to define
other useful types of lists, as we shall see next.

Ground Types
------------

The usual definition of the recursive list type can be decomposed into
the three finer steps:

   #. Abstracting over the self type.
   #. Instantiating the abstract type by self type.
   #. Equating the instance with the self type to close the loop.

As in:

.. code:: ocaml

   (** Defining the ground list type from the abstract type *)
   module MyList = struct
     type ('a, 'b) t = Nil | Cons of 'a * 'b   (* 1 (step 1) *)
     type 'a ground = ('a, 'b) t
         constraint 'b = 'a ground             (* 2 (steps 2 & 3) *)
   end

Equation ``(* 1 *)`` is for step 1. Equation ``(* 2 *)`` is for steps 2
and 3: if you instantiate ``'b`` with ``'a ground`` in ``(* 1 *)``, you
would get (literally):

::

   type ('a, 'a ground) t = Nil | Cons of 'a * 'a ground  (* 1b *)

Then by ``(* 1b *)`` and ``(* 2 *)`` we have:

.. code:: ocaml

   type 'a ground = Nil | Cons of 'a * 'a ground  (* 2b *)

Equation ``(* 2b *)`` is the usual definition of a list type, which we
call a *ground list type*.

.. The equation ``(* 2b *)`` is recommended way to define your own types. We should mention that you can use a little bit longer syntax that will give the same result but

The abstract list type can also be used to define logic list types.

.. note::

   The type definition `type 'a ground = ('a, 'a ground) t` above require an non-conventional compiler switch `-rectypes`. It allows more liberal types definitions by disable infamously known in the area of logic programming "occurs check". Without this switch
   we can't not construct a type ground list which type system considers equal to predefined ``Stdlib.list`` .
   While using this switch we pay by seeing less trivial type errors in compile time.

.. todo::

   Actually in the moment we can't declare the list type which is the same as predfined one. We need a small patch.

Logic Types
-----------

In a relational program, a list engages with logic variables (like
``X, Y, Z``, capitalized as in Prolog) in cases like:

#. ``Cons (1,Nil)`` and ``Nil`` — No logic variable occurrence at all. The lists are actually ground.
#. ``Cons (X, Nil)`` and ``Cons (X, Cons (Y, Nil))`` and ``Cons (1, Cons (X, Cons (Y, Nil)))`` — There are only unknown list members.
#. ``Cons (1,Y)`` — There is only an unknown sub-list.
#. ``Cons (X,Y)`` and ``Cons (X, Cons (Y, Z))`` and ``Cons (X, Cons (3, Cons (Y, Z)))`` — There are both unknown list members and an unknown sub-list.
#. ``X`` — The list itself is wholly unknown.

Due to possible presence of logic variables in various ways shown above,
the concept of a list in a relational program is more general than the
concept of a ground list. We call them *logic lists*, for which we now
define a type.

Observe that for cases 1-4, we have some knowledge about the structure
of the list: we know whether it is empty or not because there is a top
level constructor to inspect. We call such logic lists *guarded*.

.. todo::

   I would recommend to use the term *partially ground* instead of *guarded*. What do you think, Yue Li?

But
for case 5, we have no idea about the structure of the list for there is
no top level constructor to provide a clue: we call it a *pure logic
list*, which is just a logic variable. This is an important distinction
needed for typing logic lists, and we summarize it as follows:

.. code:: ebnf

   logic list          = pure logic list
                       | guarded logic list;

   pure logic list     = logic variable;

   guarded logic list  = 'Nil'
                       | 'Cons', '(', logic list member, logic list, ')';

The type for a (polymorphic) logic list can then be implemented with
mutual recursion as follows:

.. code:: ocaml

   (** A logic list type definition *)
   type 'b logic_list  =  Value of 'b guarded_logic_list
                       |  Var   of int * 'b logic_list list
   and  'b guarded_logic_list  = ('b, 'b logic_list) MyList.t

where the constructors ``Value`` and ``Var`` are used to distinguish a
guarded logic list from a pure logic list. Moreover, The ``Var``
constructor’s ``int`` argument uniquely identifies a pure logic list,
and the second argument is a (possibly empty) list of logic lists that
can be used to instantiate the pure logic list.

.. todo::

   Say explicilty about disequalty constraints

.. todo::

   Discuss with Yue Li why concept of guarded types is 'illuminating'.

.. raw:: html

   <hr>

**Example.** Below are some inhabitants of the type ``int logic_list`` :

.. code:: ocaml

   (** case 1: a guarded logic list *)
   Value Nil
   (** case 1: a guarded logic list which is an integer
    *  cons'ed to another guarded logic list *)
   Value (Cons (1, Value Nil))
   (** case 3: a  guarded logic list which is an integer
     * cons'ed to a pure logic list*)
   Value (Cons (1, Var (1,[])))
   (** case 5: a pure logic list *)
   Var (1,[])

In all examples above we could see that the inhabitants are logic lists where logic variables
may only denote unknown sub-lists. This is because the parameter of
``logic_list`` is instantiated by a ground type (``int``). To allow
logic variables as list members (as in cases 2 and 4), we need to define
the type of *logic number* and use it as the type parameter instead of
``int``, as follows.

.. raw:: html

   <hr>

We define the Peano numbers. A *Peano number* is a natural number
denoted with two symbols ``O`` and ``S`` with auxiliary parentheses
``()``. The symbol ``O`` is interpreted as the number zero, and the
symbol ``S`` a successor function. Then the number one is denoted
``S(O)``, two ``S(S(O))``, three ``S(S(S(O)))`` and so on. Peano numbers
are frequently used in relational programming, where they appear like: -
``O``, ``S(O)`` — Ground (Peano) numbers. - ``X``, ``S(X)``, ``S(S(X))``
— Numbers with a logic variable ``X``.

Regarding all these as *logic numbers*, we distinguish:

- ``X`` — The pure logic number.
- ``O``, ``S(O)``, ``S(X)``, ``S(S(X))`` — Guarded logic numbers.

We can define abstract, ground and logic Peano number types as well:

.. code:: ocaml

   (** Abstarct, ground and logic Peano number types *)
   module Peano = struct
     type 'a t    = O | S of 'a             (** Abstract *)
     type ground  = ground t                (** Ground *)
     type logic   = Value of guarded        (** Logic  *)
                  | Var of int * logic list
     and  guarded = logic t                 (** ... and Guarded *)
   end

.. .. note::
..
..    For peano numbers we declare constructors ``Var`` and ``Value`` that are distinct from the ones from List module. In real OCanren implementation these two constructors belog to ``OCanren.logic`` type.

Similar to logic lists, a logic number is either

- a pure logic number (e.g., ``X``), or
- a guarded logic number that is either ``O`` or ``S`` applied recursively to a logic number.

Pure and guarded logic numbers are again distinguished using constructors ``Var`` and ``Value`` respectively.

.. raw:: html

   <hr>

**Example.** Below are some inhabitants of the type ``Peano.logic`` :

.. code:: ocaml

   (** a pure logic number X *)
   Var (1,[])
   (** a guarded logic number which is the constructor [O] *)
   Value O
   (** a guarded logic number S(X) which is the constructor [S] applied to
      a (pure) logic number X *)
   Value (S (Var (1,[])))
   (** a guarded logic number S(O) which is the constructor [S] applied to
      a (guarded) logic number which is the constructor [O] *)
   Value (S (Value O))
   (** a guarded logic number S(S(X)) *)
   Value (S (Value (S (Var (1,[])))))


Then the type ``Peano.logic logic_list`` has the following inhabitants:

.. code:: ocaml

   Value Nil                                       (* case 1 *)
   Value (Cons (Value (S (Value O)) , Value Nil))  (* case 1 *)
   Value (Cons (Var (1,[]), Value Nil))            (* case 2 *)
   Value (Cons (Value (S (Value O)) , Var (2,[]))) (* case 3 *)
   Value (Cons (Var (1,[]), Var (2,[])))           (* case 4 *)
   Var (1,[])                                      (* case 5 *)

Therefore, when we talk about a list of numbers in relational
programming, we are actually talking about a logic list of logic
numbers.

.. raw:: html

   <hr>

More abstraction over logic types
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Compare the types of logic lists and logic numbers (reproduced below):

.. code:: ocaml

   (* Comparing the types of logic lists and logic numbers *)

   (* The logic list type *)
   type 'b logic_list  =  Value of 'b guarded_logic_list
                       |  Var   of int * 'b logic_list list
   and  'b guarded_logic_list  = ('b, 'b logic_list) MyList.t

   (* logic number type. Excerpt from module Peano *)
   type logic   = Value of guarded
                | Var of int * logic list
   and  guarded = logic t


We could see that they both involve the constructors ``Value`` and
``Var`` with similar argument structures: the ``Value`` constructor’s
argument is always a guarded type, and the ``Var`` constructor’s first
argument is always ``int`` and second argument is always a ``list`` of
the logic type itself. This imlpies that we can extract these common
parts for reuse , by equating them to a new type constructor with one
type parameter that abstracts from the guarded types, as follows:

.. code:: ocaml

   (** The new, reusable type constructor for defining logic types *)
   module MyLogic = struct
     type 'a logic = Value of 'a | Var of int * 'a logic list
   end

Next time when we what to define ``('a1, ..., 'an) Something.logic``,
instead of writing:

.. code:: ocaml

   (** longer logic type definition  *)
   module Something = struct
     type ('a1, ..., 'an, 'self) t = (* ... type information omitted *)
     type ('a1, ..., 'an) logic = Value of ('a1, ..., 'an) guarded
                                | Var of int * ('a1, ..., 'an) logic list
     and ('a1, ..., 'an) guarded = ('a1, ..., 'an, ('a1, ..., 'an) logic) t
   end

we could write:

.. code:: ocaml

   (** shorter logic type definition  *)
   module Something = struct
     type ('a1, ..., 'an, 'self) t = (* ... type information omitted *)
     type ('a1, ..., 'an) logic =  ('a1, ..., 'an) guarded MyLogic.logic
     and ('a1, ..., 'an) guarded = ('a1, ..., 'an, ('a1, ..., 'an) logic) t
   end

for we can derive the longer from the shorter using ``MyLogic`` (the
reader may write down the derivation as an exercise). As examples: the
logic list type can be rewritten as:

.. code:: ocaml

   (** Defining the logic list type using [MyLogic.logic] *)
   module MyList = struct
     type ('a, 'b) t = Nil | Cons of 'a * 'b
     type 'b logic   =  'b guarded MyLogic.logic
     and 'b guarded  = ('b, 'b logic) t
   end

and the logic number type as:

.. code:: ocaml

   (** Defining the logic number type using [MyLogic.logic] *)
   module Peano = struct
     type 'a t   = O | S of 'a
     type logic  =  guarded MyLogic.logic
     and guarded = logic t
   end

Or even shorter, skipping the guarded types:

.. code:: ocaml

   (** Concise definitions of abstract and logic types
       for lists and Peano numbers *)

   module MyList = struct
     type ('a, 'b) t = Nil | Cons of 'a * 'b
     type 'b logic   =  ('b, 'b logic) t MyLogic.logic
   end

   module Peano = struct
     type 'a t   = O | S of 'a
     type logic  =  logic t MyLogic.logic
   end

Injected Types
--------------

The ``injected`` type constructor collects the corresponding ground and
logic type constructors, to which we assign the name ``groundi`` (read
“groun-dee”):

.. todo::

   Rename groundi to injected in the source code, and in the tutorial after that


.. code:: ocaml

   (** Complete definitions of injected types
       for lists and Peano numbers *)

   module MyList = struct
     type ('a, 'b) t = Nil | Cons of 'a * 'b
     type 'a ground = ('a, 'a ground) t
     type 'b logic =  ('b, 'b logic) t MyLogic.logic
     type ('a, 'b) groundi = ('a ground, 'b logic) injected
   end

   module Peano = struct
     type 'a t = O | S of 'a
     type ground = ground t
     type logic =  logic t MyLogic.logic
     type groundi = (ground, logic) injected
   end

The ``injected`` type constructor is abstract in the sense that its type
information is hidden from the user. Therefore we do not concern
ourselves as to what an inhabitant of an injected type looks like.

Injecting non-recursive types
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This is even simpler: no need to abstract over self.

.. The consequence is that the abstract type and the ground type coincide (and the guarded type as well if made explicit).

.. \*\* I think things are a little bit more complicated. Fully abstract
.. type coincide with ground only if type is fully abstract from the
.. beggining. If a type definition uses some predefined types in it, we
.. will still need a fully abstract type, even where this type definition
.. is not recursive*\*

For example, logic pairs:

.. code:: ocaml

   module MyPair = struct
      type ('a1, 'a2) t = 'a1 * 'a2
      type ('a1, 'a2) ground = ('a1, 'a2) t
      type ('b1, 'b2) logic =  ('b1, 'b2) t MyLogic.logic
      type ('a1, 'a2, 'b1, 'b2) groundi = (('a1, 'a2) ground, ('b1, 'b2) logic) injected
   end

We can now talk about:

.. code:: ocaml

   (** Pair of Peano numbers *)
   module PP = struct

     (** Ground pairs of ground Peano numbers, like (O, O) and (O, S(O)) *)
     type ground = (Peano.ground, Peano.ground) MyPair.ground

     (** Logic pairs of logic Peano numbers, like (X, S(Y)), Y and (X, X) *)
     type logic = (Peano.logic, Peano.logic) MyPair.logic

     (** Injected pairs of Peano numbers (abstract type) *)
     type groundi = (Peano.ground, Peano.ground, Peano.logic, Peano.logic) MyPair.groundi
               (* = (ground, logic) injected *)

   end

   (** Peano number * Peano number list --- Pairs *)
   module PPL = struct
     type ground = (Peano.ground, Peano.ground MyList.ground) MyPair.ground
     type logic  = (Peano.logic,  Peano.logic MyList.logic) MyPair.logic
     type groundi = (* = (ground, logic) injected *)
       (Peano.ground,
        Peano.ground MyList.ground,
        Peano.logic,
        Peano.logic MyList.logic) MyPair.groundi
   end

As an exercise, the reader may define the injected types for pairs of
polymorphic lists, and lists of polymorphic pairs.

Injecting non-regular recursive types
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A non-regular recursve type is a parameterized type constructor in whose
recurisve definition at least one type parameter is instantiated (See
also
`this <https://ocaml.org/releases/4.11/htmlman/polymorphism.html#s:polymorphic-recursion>`__).
Injection of non-regular recursive types is not discussed here, and, frankly speaking, never required in relational progrmming in OCanren.

Compiling the Program
---------------------

The types that we learnt in this lesson are put together in the file
`digTypes.ml <digTypes.ml>`__ which can be compilied successfully using
the lightweight `Makefile <Makefile>`__, where we need the ``-rectypes``
compiler option to deal with the rather liberal recurisve types that
appear in this lesson.


The use of ``MyLogic.logic`` and ``MyLogic.injected`` instead of (resp.) ``OCanren.logic`` and ``OCanren.injected``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Note that we defined the module ``MyLogic`` for pedagogical purposes
only, so that we do not have to refer to the OCanren package during
compilation. The reader is encouraged to find the corresponding
definitions in the OCanren module
`Logic <../../Installation/ocanren/src/core/Logic.mli>`__ by himself.

Conclusion
----------

OCanren works on injected types that are defined via abstract, ground
and logic types. The table below organizes these types into four levels
by dependency.

========= ==============
Level No. Level Name
========= ==============
1         Fully Abstract
2         Ground
3         Injected
4         Logic
========= ==============

In principle, OCanren can be implemented without injected types by performing unification on logic types. But it will hurt the performance a lot. Detailed motivation about 'injected' typed they can get in the  :ref:`paper <papers>` `Typed Embedding of Relational Programming Language`.

We give templates for definig injected types:

.. code:: ocaml

   open OCanren

   (** Template of an injeced, regular recursive type *)

   module Something = struct
     type ('a1, ..., 'an, 'self) t = (* ... add type information here *)
     type ('a1, ..., 'an) ground = ('a1, ..., 'an, ('a1, ..., 'an) ground) t
     type ('b1, ..., 'bn) logic =  ('b1, ..., 'bn, ('b1, ..., 'bn) logic) t OCanren.logic
     type ('a1, ..., 'an, 'b1, ..., 'bn) groundi = (('a1, ..., 'an) ground, ('b1, ..., 'bn) logic) injected
   end

   (** Template of an injeced, non-recursive type *)

   module Something = struct
     type ('a1, ..., 'an) t = (* ... add type information here *)
     type ('a1, ..., 'an) ground = ('a1, ..., 'an) t
     type ('b1, ..., 'bn) logic =  ('b1, ..., 'bn) t OCanren.logic
     type ('a1, ..., 'an, 'b1, ..., 'bn) groundi = (('a1, ..., 'an) ground, ('b1, ..., 'bn) logic) injected
   end

The reader may apply these templates to define his own types. OCanren is
for typed relational programming.Two points here: it is typed, and it is
relational. We have now studied how to work with the types. This clears
the way so that we can then focus on the relational part.

Allusion to OCanren standard libraries
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

As examples, we defined types of Peano numbers, and polymorphic lists
and pairs, each showing the four-level structure. The ``Peano``,
``MyList`` and ``MyPair`` modules correspond to the OCanren `standard
libraries <../../Installation/ocanren/src/std>`__ ``OCanren.Std.Nat``, ``OCanren.Std.List``
and ``OCanren.Std.Pair`` respectively.
