A Simple Data Base
==================

In this lesson we learn the basic form of relational programming:
defining and querying a data base. In particular, we build a toy data
base of the 32 control characters in the ASCII table, associating each
character with its integer number and description, for example: ``BS``
with number 8 and description “Back space”.

The `program <ASCII_Ctrl_DB.ml>`__ is a bit long, yet simple in the
sense that the relation defined therein is not recursive: it is a
straightforward listing of the data base, which is the very basic form
of relational programs.

Reading the Program
-------------------

The structure of the program is as follows:

#. The initial opening statement
#. Type definitions and utilities
#. The ASCII control characters type - Injection utilities
#. The logic string type
#. The data base as a relation ``ascii_ctrl``
#. Some queries on the database

Read the definition of ``ascii_ctrl`` as:

Values *c*, *n* and *s* form the
relation *ascii_ctrl* iff *c* is NUL and *n* is 0 and *s* is the string
“Null”, or *c* is SOH and *n* is 1 and *s* is the string “Start of
heading”, or …, or *c* is US and *n* is 31 and *s* is the string “Unit
separator”.

Read the query:

.. code:: ocaml

   (** Find the control characters in a given range *)
   let _ =
     List.iter print_endline @@
       Stream.take ~n:18 @@
         run q (fun s ->
             ocanren {fresh c,n in Std.Nat.(<=) 0 n
                                   & Std.Nat.(<=) n 10
                                   & ascii_ctrl c n s}) project

as: Print at most 18 possible values of *s*, such that exist some *c* and *n* where *n* ranges from 0 to 10 inclusive, and the tuple *(c, n, s)* satisfies the relation *ascii_ctrl*.

OCanren will print the following (eleven) strings:

::

   Null
   Start of heading
   Start of text
   End of text
   End of transmission
   Enquiry
   Ackonwledge
   Bell
   Back space
   Horizontal tab
   Line Feed

We could see that the relational program specifies a relation, and it
has been used to find missing elements of a tuple that is claimed to
satisfy some constraint of which the relation is a part. In so doing, we
did not tell the program *how* to find these missing elements. It was
the semantics of the programming language that did this automatically.
We explain the syntax and semantics next.

Syntax of a Formula
-------------------

The notion of a formula in OCanren is different from that in logic
programming, i.e., the Horn clause subset of first-order predicate
logic. Instead it is quite close to formulae in the system
`μMALL <https://doi.org/10.1007/978-3-540-75560-9_9>`__.

A formula is either atomic, or is compound and built from atomic
formulae using conjunction (``&``), disjunction (``|``) and existential
quantification (``fresh``). Atomic formulae are built from predicate
symbols followed by their arguments. There are only two predicate
symbols ``==`` and ``=/=``. A formula is allowed to be infinitely long.
Formulae can be abbreviated by finitely-represented (possibly recurisive) definitions.

**Example.** Atomic, compound, named and infinite formulae:

- ``x == y`` and ``1 =/= 2`` are two atomic formulae.
- By the definition ``foo x y :=  x == 1 & y =/= 2``, we can use ``foo x y`` to abbreviate the compound formula ``x == 1 & y =/= 2``.
- By the recursive definition ``is_nat x := x == O | fresh y in x == S y & is_nat y`` we can use ``is_nat x`` to abbreviate the infinitely long formula:

  .. code:: ocaml

    x == O
    | fresh y1 in
      x == S y1 & { y1 == O
                  | fresh y2 in
                    y1 == S y2 & { y2 == O
                                 | fresh y3 in
                                   y2 == S y3 & { ... }}}

We now give the concrete syntax of a formula in OCanren.

.. code:: ebnf

   formula  = atomic formula
        | compound formula
        | named formula
        | '{', formula, '}' ;

   atomic formula = value, '==', value | value, '=/=', value;

   compound formula = formula, '&', formula
                    | formula, '|', formula
            | 'fresh', lparams, 'in',  formula;

   named formula = formula name, ' ', values;

   formula name definition = 'let', ['rec'], let-binding, {'and', let-binding};

   let-binding =  formula name, [':', typexpr, '->', 'goal' ], '=',
                  'fun', fparams, '->', 'ocanren','{', formula, '}' ;

   lparams = param, {',', param};
   fparams = param, {' ', param};
   values  = value, {' ', value};

The scope of ``fresh...in`` extends as far as possible. ``&`` binds
tighter than ``|``. A formula always has the type ``goal`` (this type
constructor is provided by the module Core). The braces ``{}`` could be
used for explicit grouping, as in ``{ x == 1 | x == 2 } & y == 0``.

A Note on the Concept of a *Goal*
---------------------------------

In logic programming, we call the formula which we want to refute a
*goal*. This term (i.e., goal) is inherited by the modern successor of
logic programming, which is called *relational programming*. However,
the semantics of a *goal* nevertheless changes: it is no longer
something that we want to refute, but something for which we want to
find variable substitutions so that it is true. In other words:

  - Logic programming is proof by contradiction: we want to find variable substitutions so that a formula *F* is true, but what we do is to find substitutions so that the negation of F is false.
  - Relational programming is proof by straightforward construction without the logical detour of “negation of negation”.

The Semantics of a Formula
--------------------------

A formula has two semantics: the *declarational semantics* and the
*operational semantics*. The way in
which the reader is advised to read the relation definition and the
query is actually part of the declarational semantics. The operational
semantics concerns how the answers shall be searched for (mechanically),
which is part of the implementation of the language.

Declaratively, the two predicate symbols ``==`` and ``=/=`` means
respectively “syntactic equality” and “syntactic disequality”. The logic
connectives mean as usual, and a value just denote itself as a syntactic
object. The operational semantics of OCanren is a set of stream
manipulation rules attached to the logic connectives and the predicate
symbols, and formulae are viewed as functions taking a stream member as
input and returning a stream. We explain the operational semantics of
OCanren in more detail below. Firstly the concept of a *stream*.

Streams
~~~~~~~

A stream is a generalization of a list: members of a list can be put on
one-on-one correspondence with members of some *finite* subset of the
natural numbers, whilst members of a stream can be put on one-on-one
correspondence with members of some possibly infinite subset of the
natural numbers. Intuitively, the imaginary, infinitely long sequence of
all natural numbers itself is an example of a stream. The sequence of
all integers ``...-3 -2 -1 0 1 2 3...`` is a stream too, equivalently
``0 1 -1 2 -2 3 -3 ...`` is a stream of integers too, but they are in different order than in previous stream.

The set of all streams can also be defined in the more technical,
*coinductive* manner as follows:

#. Let **FS** be an operator whose input is a set of sequences and     whose output is also a set of sequences. A sequence is said to be     composed of its members drawn from a set of possible members.

#. The output of **FS** is constructed by:

   #. Starting with an empty set, to add members to it incrementally;

   #. Adding the empty stream;

   #. Extending each sequence of the input set with an arbitrary member, then adding the results.

#. The set St of all streams is the *largest* set that is a fixed-point of **FS**, in other words, **FS** (St) = St and St is a superset of st for all **FS** (st) = st.

**Example** If we restrict sequence members to integers, and let the
input be ``{123, 111}``, which is the set whose members are the
sequences ``123`` and ``111``. One possible output of **FS** operating
on the input is ``{e, 0123, 5111}`` where ``e`` is the empty stream.
Another possible output is ``{e,1123, 1111}``. In neither case the
output equals the input, which is quite usual. The two notable
exceptions are the set Lmin of all lists of integers, and the set Lmax
of all finite and infinite sequences of integers. They are both
fixed-points of **FS**, known as the *least fixpoint* and the *greatest
fixpoint*. Lmax is also the set of all streams of integers.

Note that in a typical inductive specification we could require that the set being defined is the samllest fixed-point. Here instead we ask for the *largest*, hence the *coinductive manner*.

.. todo::

  It looks like very complex description of a stream but maybe it is only for me

Substitution
~~~~~~~~~~~~

A *substitution* is a list of substitution components. A *substitution
component* (for short: *component*) is a pair (*lvar*, *value*) where
*lvar* is a logic variable. A substitution component (*lvar*, *value*)
can be *applied* to some value *valuepre*, so that all occurrences of
*lvar* in *valuepre* are simultaneously replaced by *value*, and the
result is another value *valuepost*. A component is *applicable* if
applying it would make a difference. To apply a substitution is to
repeatedly apply its components until none is applicable.

**Example** Applying ``[(x, Cons(1,y)); (y, Cons(2,z)); (z, Nil)]`` to
``Cons(0,x)`` results in: ``Cons(0,Cons(1,Cons(2,Nil)))``.

Formulae as Stream Builders
~~~~~~~~~~~~~~~~~~~~~~~~~~~

A formula is a stream builder as far as the operational semantics is
concerned. It takes a substitution :math:`subst_{in}` as input and returns a
stream of substitutions as output:

:math:`subst_{in} \xrightarrow{\text{formula}} subst_{out}, subst_{out}, subst_{out}, …`

For each substitution *substout* in the returned stream, applying the
concatenation *substin ^ substout* makes the formula true in the sense
of the declarational semantics.

.. todo::

  Yue Li, what you meant saying 'applying concatenation'?

**Example.** Given as input the empty substitution ``[]`` :

- The formula ``x == Cons(1, Nil)`` returns the stream that consists of the substitution ``[(x, Cons(1,Nil))]``.
- The formula ``x == Cons(1, Nil) & y == Cons(2, x)`` returns the stream that consists of the substitution ``[ (x, Cons(1,Nil)); (y, Cons(2,x)) ]``.
- The formula ``is_nat x`` returns the stream that consists of the substitutions ``[(x, O)]``, ``[(x, S(y1));(y1, O)]``, ``[ (x, S(y1)); (y1, S(y2)); (y2, O) ]``, ...
- The formula ``1 == 1`` returns the stream whose only member is ``[]``.
- The formula ``1 == 2`` returns the empty stream: there is no way to make the formula true.

Disjunction as stream interleaving
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

**Example.** Let :math:`s_1` denote the stream of all positive intergers, and :math:`s_2` the stream of all negative intergers. The result of interleaving :math:`s_1` with :math:`s_2`, denoted :math:`s_1 |zip| s_2` is ``1, -1, 2, -2, ...``, and :math:`s_2 |zip| s_1` is ``-1, 1, -2, 2, ...``.

The disjunction :math:`F_1 \mid F_2` of two formulae :math:`F_1`, :math:`F_2` is itself a formula on the top level, so it is a stream builder, taking a
substitution as input and returns a stream of substitutions. It builds
the output stream by interleaving the two streams built separately by :math:`F_1` and :math:`F_2`, both of which share the same input as their immediate top level formula. In more formal terms:

.. math::

  (F_1 \mid F_2)\ substin\quad =\quad (F_1\ substin)\ |zip| \ (F_2\ substin)

Every substitution from the output stream (concatenated with the input)
makes either of the two disjuncts true.

Conjuction as a Stream Map-Zipper
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

To *map-zip* a stream builder *F* with a stream :math:`s := m_1, m_2, m_3, ...` (denoted :math:`F\ mzip\ s`), is to apply :math:`F` individually to each
member :math:`m_k` of the stream, resulting in streams :math:`s_k`, and then zip all :math:`s_k` together.

        .. math::
          :nowrap:

          \begin{eqnarray*}
              & F mzip s\\
            =& F\ mzip\ m_1,\ m_2,\ m_3, \dots\\
            =& F\ m_1 zip\ (F\ m_2\ zip\ (F m_3\ zip\ (…))) \\
            =& s_1\ zip\ (s_2\ zip (s_3\ zip (…)))
          \end{eqnarray*}

**Example.** Let F be a stream builder that works like this: :math:`F\  n = n,n,n,…` Then:

        .. math::
          :nowrap:

          \begin{eqnarray*}
             & F\ mzip\ 1,2,3 \\
            =&  F_1\ zi\ (F_2\ zip\ F_3) \\
            =& 1,1,1,…\ zip\ (2,2,2,…\ zip\ 3,3,3,…) \\
            =& 1,1,1,…\ zip\ 2,3,2,3,… \\
            =& 1,2,1,3,1,2,1,3, …
          \end{eqnarray*}

A conjunction :math:`F_1 & F_2` provides the input substitution to :math:`F_1` first, and then map-zips the output of :math:`F_1` with :math:`F_2` :

        .. math::
           :nowrap:

           (F_1\ &\ F_2) subst_{in}\ =\ F_2 mzip ( F_1 subst_{in})

Every substitution from the output stream (concatenated with the input)
makes both of the two conjuncts true.

Working with GT and Camlp5
--------------------------

We use packages GT and Camlp5 in OCanren programs. The influence of GT Camlp5 syntax extension is that we can use the ``@type`` syntax to define types, which convenienty generates useful functions for the defined type, for example, a *show* function that converts values of the defined type into a string, which we use to print the result of a query. Camlp5 expands the content of the ``ocanren{}`` quotation, allowing us to write readable code.

The @type syntax
~~~~~~~~~~~~~~~~

In OCanren, type constructors are often defined by :

.. code::

   type definition = '@type', typedef, 'with', plugins

   plugins = plugin { ',' , plugin }

   plugin  ::= 'show' | 'gmap' | etc

where the syntactic category ``typedef`` is the same as
`that <https://ocaml.org/releases/4.11/htmlman/typedecl.html>`__ of
OCaml, and the category ``etc`` signifies omission: the most frequently
used plugins in OCanren are *show* and *gmap*, providing for the defined
type a string conversion function (like
`Stdlib.string_of_int <https://ocaml.org/releases/4.11/htmlman/libref/Stdlib.html>`__)
and a structure preserving map function (a generalization of
`List.map <https://ocaml.org/releases/4.11/htmlman/libref/List.html>`__)
respectively. The other less used plugins are not shown here.

A type definition of the form ``@type <typedecl> with <plugins>`` is
expanded at the syntactic level by GT into:

#. A type definition of the usual form ``type <typedecl>``, where the value of ``<typedecl>`` is preserved, and
#. Several (auto-generated) plugin definitions.

The effect of syntactic transformation, including what the ``@type``
definitions become after expansion, can be viewed by adding the “dump
source” option ``-dsource`` in the Makefile as explained in a comment
line there. For instance, the ``String`` module:

.. code:: ocaml

    (** {2  The logic string type} *)
   module String = struct
     @type t = GT.string with show
     @type ground = t with show
     @type logic = t OCanren.logic with show
     type groundi = (ground, logic) injected
   end

would be expanded into `this <lstring.ml>`__, where we could see that
besides the type constructor definitions a lot more codes have actually
been auto-generated to support any GT plugin that the user may request.

Note in the ``LString`` module that the type constructor name ``string``
is qualified by the module name ``GT``, for we need to use the GT
version of the string type which provides the useful plugins and
otherwise it is the same as the OCaml built-in string type. Plugins are
(auto-)created inductively: GT provides plugins for base types and rules
for building plugins for compound types from component types.

.. todo::

  Write properly part about syntax extensions and port this part there

.. todo::

  I will clarify this a bit. We do not use the GT version of
  ``string`` type, in reality it is a just type alias:
  ``module GT = struct type string = Stdlib.string ... end``. What is
  really happening here, is that functions for showing and gmapping string
  type are located in module GT. So we need 1) either write ``GT.string``
  instead of ``string`` and preprocessor will generate
  ``GT.show GT.string`` instead of ``GT.show string``, 2) or make
  ``open GT`` somewhere about and use type ``string`` without fully
  qualified name. \*\*

The injection functions and the ``ocanren {...}`` quotation
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The signature of the ``ASCII_Ctrl.Inj`` module shall explain itself. For
every value constructor, an accompanying injection function shall be
defined (either by the user or auto-generated by the tool
`noCanren <https://github.com/Lozov-Petr/noCanren>`__), whose name shall
be the same as the constructor name except that the first letter is set
to lower case. In the ``ocanren{...}`` quotation, wherever a value
constructor occurs, its corresponding injection function is implicitly
called. Hence the ``let open ASCII_Ctrl.Inj in`` statement that preceeds
the body of the ``ascii_ctrl`` relation. The quotation in the body of
``ascii_ctrl`` is expanded as follows:

.. code:: ocaml

   let ascii_ctrl =
     (fun c n s ->
        let open ASCII_Ctrl.Inj in
        OCanren.disj
          (OCanren.conj (OCanren.unify c (nUL ()))
              (OCanren.conj (OCanren.unify n (OCanren.Std.nat 0))
                (OCanren.unify s (OCanren.inj (OCanren.lift "Null")))))
          (OCanren.disj
              (OCanren.conj (OCanren.unify c (sOH ()))
                (OCanren.conj (OCanren.unify n (OCanren.Std.nat 1))
                    (OCanren.unify s
                      (OCanren.inj (OCanren.lift "Start of heading")))))
              (OCanren.disj
                (OCanren.conj (OCanren.unify c (sTX ()))
                    (OCanren.conj (OCanren.unify n (OCanren.Std.nat 2))
                      (OCanren.unify s
                          (OCanren.inj (OCanren.lift "Start of text")))))
   (* ... etc *)

The above code excerpt is also from what is displayed on the terminal
after compiling the source with the “dump source” option ``-dsource``.

Conclusion
----------

A program in OCanren is understood with respect to its syntax and
semantics. We define types in four levels, using the ``@type`` syntax of
GT. We define injection functions for value constructors. We then define
formulae in OCanren formula syntax, which are put in the ``ocanren{}``
quotation powered by Camlp5. In set theory when we think about a
relation, we are actually thinking about a function *R* that can be
applied to its arguments and return either true or false, like this:

  :math:`arg_1, …, arg_n \rightarrow R \rightarrow true\ |\ false`

But in relational programming, when we think about a relation *R*, the
most important thing is not that *R* is a function, but :math:`R(arg_1, …, arg_n)` *in whole* is a function, i.e., we regard what is known by
logicians as a formula, as a function whose input is an initial variable
substitution and whose output is the set of all possible variable
substitutions where each member when combined with the initial
substitution makes the formula true, like this:

  :math:`subst_{in} \rightarrow  R(arg_1, …, arg_n) \rightarrow subst_{out}, subst_{out}, subst_{out},...`
