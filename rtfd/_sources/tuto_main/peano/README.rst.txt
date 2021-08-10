A Library for Peano Arithmetic
==============================

We hope the reader will learn the following techniques (labeled as
**T.1**, **T.2**, etc) from this lesson: -
`T.1 <#t1-advanced-injection-functions>`__ Defining injection functions
for value constructors of variant types, using the Fmap family of module
functors ``Fmap``, ``Fmap2``, ``Fmap3``, etc., which are provided by the
module `Logic <../../Installation/ocanren/src/core/Logic.mli>`__. -
`T.2 <#t2-reification-and-reifiers>`__ Defining reifiers to convert data
from the injected level to the logic level, again with help from the
Fmap family of module functors. -
`T.3 <#t3-overwriting-the-show-function>`__ Overwriting, or redefining
the “show” function for values of a logic type, to allow for more
concise and human readable printing of them. -
`T.4 <#t4-relations-on-peano-numbers>`__ Defining (possibly recursive)
relations, e.g., comparison, addition and division on Peano numbers. -
`T.5 <#t5-scrutinizing-relations>`__ Making queries to relations using
combinations of unknown arguments. -
`T.6 <#t6-analyzing-the-search-behaviour>`__ Analyzing why a query
returns certain answers. - `T.7 <#t7-modifying-the-search-behaviour>`__
Reordering the conjuncts within the body of a relation definition to
modify the way in which the relation searches for answers in a given
query. - `T.8 <#t8-the-trick-of-generate-and-test>`__ Programming a
relation so that answers to certain queries are found by brute-force. -
`T.9 <#t9-the-formula-parser>`__ Observing that the implementation of
the ``ocanren {}`` quotation takes care of the precedence, associativity
and scope of the logic connectives, and replaces constructors of variant
types by injection function names, and primitive values by their
injected versions. - `T.10 <#t10-building-a-library>`__ Writing and
testing a library in OCanren.

The techniques are presented in detail in sections below, to which the
labels ( **T.1**, **T.2**, etc) are linked. Each section is
self-contained and could be read independent of other sections.

The library has a systematic test file, which can be compiled (and
linked) and executed by running the following shell commands in (your
local copy of) the lesson directory:

::

   make && ./peano.opt

A copy of the result of the test is `answers.txt <answers.txt>`__ that
is obtained using the shell command ``./peano.opt > answers.txt``.

(T.1) Advanced Injection Functions
----------------------------------

The primary injection operator is ``!!`` which is used to cast primitive
values (such as characters and strings) and constant constructors of
variant types (particularly whose type constructors do not have a type
parameter) from the ground level to the injected level. For those
variant types whose type constructors have one or more type parameters,
the primitive injection operator is inadequate. We use instead *advanced
injection functions* to build injected values, which are defined using
distribution functions provided by the Fmap family of module functors
together with the injection helper ``inj``, all from the module Logic.
In our Peano Arithmetic `library implementation <peano.ml>`__, the
following block of code defines advanced injection functions ``o`` and
``s`` for the abstract Peano number type ``Ty.t``, which correspond
respectively to the value constructors ``O`` and ``S``:

.. code:: ocaml

   module Ty = struct
     @type 'a t = O | S of 'a with show, gmap;;
     let fmap = fun f d -> GT.gmap(t) f d;;
   end;;

   include Ty;;

   module F = Fmap(Ty);;

   let o = fun () -> inj @@ F.distrib O;;
   let s = fun n  -> inj @@ F.distrib (S n);;

The general workflow of defining advanced injection functions is as
follows:

1. We begin with a variant type whose type constructor ``t`` has one or
   more type parameters. This is always an OCanren abstract type, such
   as the abstract Peano number type or the abstract list type.
2. We count the number of type parameters of the type constructor in
   order to choose the suitable module functor from the Fmap family: for
   one type parameter, use ``Fmap``; for two type parameters, use
   ``Fmap2``; three type parameters, ``Fmap3`` and so on.
3. We request the ``gmap`` plugin for the type constructor, and use it
   to define a function named ``fmap`` simply by renaming.
4. We put the definitions of the type constructor ``t`` and the ``fmap``
   function in one module, and suppy that module as a parameter to the
   chosen Fmap family module functor. The result is a module ``F`` with
   three functions one of which is ``distrib``, the distribution
   function.
5. For each value constructor of the type ``t``, we define a function
   whose name is the same as the value constructor except that the
   initial letter is set to lower case. For example, ``Cons``, ``S`` and
   ``NUL`` become respectively ``cons``, ``s`` and ``nUL``.

\*\* Question: did you intentionally used syntax with
lambda-abstractions instead of ``let const1 x = Logic.inj ...``? \*\*

-  For each value constructor ``Constr0`` of no argument, define:
   ``ocaml      let constr0 = fun () -> Logic.inj @@ F.distrib Constr0``
-  For each value constructor ``Constr1`` of one argument, define:
   ``ocaml      let constr1 = fun x -> Logic.inj @@ F.distrib (Constr1 x)``
-  For each value constructor ``Constru`` of *u* (> 1) arguments,
   define:
   ``ocaml      let constru = fun x1 ... xu -> Logic.inj @@ F.distrib @@ Constru (x1, ..., xu)``
   In the definition of a typical advanced injection function, the value
   constructor takes arguments which are at the injected level, and the
   combination of ``inj`` and ``distrib`` serves to inject the top level
   value while preserving the structure of constructor application. If
   we explain by a schematic where a pair of enclosing square brackets
   ``[]`` signifies the injected status of the enclosed data, we would
   say that:
-  An advanced injection function ``constr`` converts a value of the
   form ``Constr ([arg1], ..., [argn])`` to a value of the form
   ``[Constr (arg1, ..., argn)]``. In other words,
-  The injection function ``constr`` takes arguments
   ``[arg1], ..., [argn]`` and builds a value of the form
   ``[Constr (arg1, ..., argn)]``.

We advise the reader to find in the
`interface <../../Installation/ocanren/src/core/Logic.mli>`__ of the
Logic module the Fmap module functor family and the functors’ argument
types (which are module types): that would provide a more formal
explanation of what advanced injection functions do and why they are
defined in the given manner.

(T.2) Reification and Reifiers
------------------------------

Say we have a logic variable ``x`` and a substitution
``[(x, Lam(z,y));(y, App(a,b))]`` that associates ``x`` with the term
``Lam(z,y)`` and ``y`` with ``App(a,b)`` where ``y, z`` are also logic
variables. We would like to know what ``x`` is with respect to the
substitution. It is straightforward to replace ``x`` by ``Lam(z,y)`` but
since ``y`` is associated with ``App(a,b)`` we can further replace ``y``
in ``Lam(z,y)``, and finally we get the term ``Lam(z,App(a,b))``.
Although there is still an unbound part ``z``, we have no further
information about how ``z`` might be instantiated, so we leave it there.
What we have done is called *reification* of the logic variable ``x``:
we instantiate it as much as possible, but allowing unbound logic
variables to occur in the result. A *reifier* is a function that reifies
logic variables.

We know that there are primary and advanced injection functions.
Similarly there are primary and advanced reifiers: the primary reifier
``Logic.reify`` reifies logic variables over base types (like character
and string) and simple variant types (i.e., those that have only
constant constructors). Advanced reifiers are for logic variables over
variant types whose type constructors have one or more type parameters
and there exist non-constant (value) constructors. The Peano Arithmetic
library defines an advanced reifier for the Peano number type:

.. code:: ocaml

   let rec reify = fun env n -> F.reify reify env n;;

Advanced reifiers are defined using the Fmap module functor family. The
correct Fmap module functor for defining the reifier for a type is the
same as that selected for defining advanced injection functions for the
same type. The result of applying the correct Fmap module functor is a
module that provides, besides a distribution function, a reifier builder
named ``reify``, e.g., ``F.reify`` in the case of our library. Note
there is an abuse of names: the name ``reify`` has been used for both
reifiers and reifier builders. If a type constructor takes other types
are parameters, then the reifier for the top level type is built from
reifiers for the parameter types: we build “larger” reifiers from
“smaller” reifiers. The Peano number reifier is recursive because the
Peano number type is recursive: the reader should refer to the
`signature <../../Installation/ocanren/src/core/Logic.mli#L136>`__ of
``F.reify`` and see how the types of the reifier and the reifier builder
fit together.

(T.3) Overwriting the *show* Function
-------------------------------------

The default *show* function for a variant type converts values of that
type to strings in a straightforward way, e.g., a logic Peano number
representation of the integer 1 would be converted to the string
``"Value(S(Value O))"`` whilst “the successor of some unknown number”
could be ``"Value(S(Var(1,[])))"``. These are not too readable.

The Logic module has already
`redefined <../../Installation/ocanren/src/core/Logic.ml#L35>`__ the
*show* function for the type ``Logic.logic`` so that the above values
would instead be converted to strings ``"S(O)"`` and ``"S(_.1)"``
respectively, omitting the verbose constructors ``Value`` and ``Var``
and displaying variables in the form ``_.n`` where ``n`` is the first
parameter of the constructor ``Var``. The redefinition happens within
the record value ``logic`` which has a field ``GT.plugins``. This record
value origins from the ``@type`` definition of the type constructor
``Logic.logic`` and is auto-generated by the GT package. The field
``GT.plugins`` is an object with several methods, one of which is
``show``: other plugins (or methods) keep their default meanings but
``show`` is redefined.

However, when there are too many repetitions of the constructor ``S``,
the *show* function as redefined in the Logic module is no longer
suitable. Our Peano Arithmetic library therefore offers a further
customized `redefinition <peano.ml#L107>`__ just for displaying logic
Peano numbers, converting those values without free variables directly
to Arabic numbers and those with free variables a sum between an Arabic
number and the symbol ``n``.

In like manner, the reader may: - Redefine the *show* function to behave
in other ways, or - Redefine other plugins by modifying the
``GT.plugins`` field, or - Redefine plugins for other types.

Some additional remarks on the last point: the ``@type`` definition of a
type constructor ``typeconstr-name`` generates a record value also named
``typeconstr-name`` of the type ``GT.t``. This could be viewed by adding
the ``-i`` option as indicated in the `Makefile <Makefile#L10>`__:

::

   BFLAGS = -rectypes -g -i

See also the `GT
source <https://github.com/JetBrains-Research/GT/blob/039193385e6cb1a67bc8a9d00c662d9d1dc5478b/src/GT.ml4#L37>`__.

(T.4) Relations on Peano Numbers
--------------------------------

This section teaches the reader how to read and write relation
definitions.

The reader is already familiar with reading and writing functions in
OCaml. To read a function, just look at the type annotation (if any) to
determine what are the input types and what is the output type, and then
inspect the function body to see how the inputs are processed to produce
the output. To write a function, first decide the type of the function,
and then design the internal procedure that produces the output from the
input.

In OCanren, a relation is a function only at the language implementation
level, and as users our experience with functions do not transfer well
when it comes to reading and writing relations. That’s why relational
programming claims the status of being a unique programming paradigm
distinct from imperative programming and functional programming. Working
with relations requires learning a new way of thinking: *declarative*
thinking.

Relation definitions are declarative, meaning that it first of all
states a proposition. The emphasize is on “what” rather than “how”. It
is the language implementation that takes care of “how”, but the user of
the language should foucs on “what”. For example, look at the addition
relation:

.. code:: ocaml

   let rec add a b c =
     ocanren{ a == O & b == c
            | fresh n, m in
              a == S n & c == S m & add n b m};;

It says nothing about how to compute the sum ``c`` of two numbers ``a``
and ``b``, instead it only says what conditions must be satisfied so
that the addition relation exists among the three numbers ``a``, ``b``
and ``c`` — if ``a`` equals ``O`` and ``b`` equals ``c``, or, if ``a``
equals ``S n`` and ``c`` equals ``S m`` and the numbers ``n,b,m`` also
satisfy the addition relation, for some ``n,m``. No other way is given
in which we can establish the addition relation among three numbers.

Another example is the “less than” relation:

.. code:: ocaml

   let rec lt a b =
     ocanren{ fresh n in
              b == S n &
                { a == O
                | fresh n' in
                  a == S n'
                  & lt n' n }};;

It says that ``a`` is less than ``b`` if there exist ``n``, such that
``b`` equals ``S n``, and either ``a`` equals ``O`` or there exist
``n'`` such that ``a`` equals ``S n'`` and ``n'`` is less than ``n``.

Other relations in the library shall be read in this way, and they are
all written with the declarative reading in mind. The reader is
encouraged to write a relation for subtraction: ``sub a b c`` iff
``a - b = c``, or, put in another way: iff ``b`` is ``O`` and ``a`` is
``c``, or ``b`` is ``S n`` and ``a`` is ``S n'`` and ``sub n' n c``.

(T.5) Scrutinizing Relations
----------------------------

Taking the “less than” relation as an example, we can ask questions
like: - Is zero less than one ? Is one less than two ? Is one less than
zero ? Is two less than one? - What is less than five ? Five is less
than what ? - What is less than what ?

The first set of questions above is for *checking*: we provide concrete
numbers and ask if they satisfy the relation. The remaining two sets of
questions are for *searching*: looking for numbers that satisfy the
relation. Note that the questions are organized: there coud be no
unknown, one unknown or two unknowns, and each argument position of the
relation might be an unknown. In general, for a relation of N arguments,
the total number of kinds of questions we can ask is ( R is the number
of unknowns in NCR):

NC0 + NC1 + NC2 + … + NCN-1 + NCN

Running the `test <test.ml#L53>`__ shows that OCanren answers all the
questions well. For example, the goal:

::

   fun q -> ocanren { lt O (S O) & lt (S O) (S(S O)) }

asks about what is ``q`` so that zero is less than one and one is less
than two, and the answer is just ``n`` meaning that ``q`` could be any
number and the relation always holds between the given numbers. The
similar goal:

::

   fun q -> ocanren { lt (S O) O | lt (S(S O)) (S O) }

asks about what is ``q`` so that one is less than zero or two is less
than one. There is no answer, meaning that there is no ``q`` to make the
relation hold between the given numbers.

The goal below asks what is less than five:

::

   fun q -> ocanren { lt q (S(S(S(S(S O))))) }

For this goal the answers ``0,1,2,3,4`` are found, which is quite
satisfactory.

The relations ``lte, add, div, gcd`` are also questioned systematically
in the test file.

Note that the addition relation can perform subtraction, and the
division relation can do multiplication. For instance, the goal below
asks “What adds 4 equals to 7 ?” and whose answer is “3”:

::

   fun q -> ocanren { add q (S(S(S(S O)))) (S(S(S(S(S(S(S O))))))) }

This amounts to performing the subtraction ``7 - 4``. The next goal asks
“What divided by 5 equals 3 with remainder 0 ?” and the answer is “15”:

::

   fun q -> ocanren { div q (S(S(S(S(S O))))) (S(S(S O))) O }

It amounts to the multiplication ``3 * 5``.

(T.6) Analyzing the Search Behaviour
------------------------------------

When asking the ``lt`` relation “what is less than 5” using the goal:

::

   fun q -> ocanren { lt q (S(S(S(S(S O))))) }                                  (G.1)

OCanren returns 0,1,2,3,4. Let’s see why. It really is a matter of
definition: we defined ``lt a b`` to be a certain formula ``(Eq.1)`` and
now we substitute 5 for ``b`` in the formula ``lt a b`` followed by
several steps of simplification then we get a formula ``(Eq.12)`` that
literally says ``a`` shall be 0, 1, 2, 3 or 4. Below are the details.

We reproduce the definition of ``lt`` in the followinig simplified form:

::

   lt a b = fresh n in b == S n & { a == O | fresh n' in a == S n' & lt n' n }
                                                                                (Eq.1)

Now replace ``b`` by ``(S(S(S(S(S O)))))`` in ``(Eq.1)``, we get:

::

   lt a (S(S(S(S(S O))))) = fresh n in (S(S(S(S(S O))))) == S n
                            & { a == O | fresh n' in a == S n' & lt n' n }
                                                                    (Eq.2)

Replace ``(S(S(S(S(S O))))) == S n`` by ``(S(S(S(S O)))) == n`` in
``(Eq.2)``, we get:

::

   lt a (S(S(S(S(S O))))) = fresh n in (S(S(S(S O)))) == n
                            & { a == O | fresh n' in a == S n' & lt n' n }
                                                                    (Eq.3)

In ``(Eq.3)``, remove ``fresh n in (S(S(S(S O)))) == n``, then replace
all free occurences of ``n`` by ``(S(S(S(S O))))``. The top level ``&``
and the braces are no longer needed, so also being removed. We get:

::

   lt a (S(S(S(S(S O))))) = a == O
                          | fresh n' in a == S n'
                    & lt n' (S(S(S(S O))))                              (Eq.4)

From ``(Eq.1)`` to ``(Eq.4)`` what we have done is to provide a concrete
value (the Peano number 5) as the second argument of ``lt`` and use the
result of unification to simplify the equation. The recursive call of
``lt`` in the right hand side of ``(Eq.4)`` can be treated similarly: we
provide a concrete value (the Peano number 4) as the second argument of
``lt`` ``(Eq.5)`` and use the result of unification to simplify the
equation ``(Eq.6)``, which is then used to substitute for the recursive
call of ``lt`` in ``(Eq.4)``, as follows.

Replace ``b`` by ``(S(S(S(S O))))`` and\ ``a`` by ``n'`` in ``(Eq.1)``
in a capture-avoiding manner, we get:

::

   lt n' (S(S(S(S O)))) = fresh n in (S(S(S(S O)))) == S n
                          & { n' == O | fresh n'' in n' == S n'' & lt n'' n }
                                                                        (Eq.5)

Using the result of unification we can simplify ``(Eq.5)`` into:

::

   lt n' (S(S(S(S O)))) = n' == O
                        | fresh n'' in n' == S n''
                  & lt n'' (S(S(S O)))                                  (Eq.6)

Now in ``(Eq.4)`` replace ``lt n' (S(S(S(S O))))`` by the right hand
side of ``(Eq.6)``:

::

   lt a (S(S(S(S(S O))))) = a == O
                          | fresh n' in a == S n'
                    & { n' == O
                               | fresh n'' in n' == S n''
                     & lt n'' (S(S(S O))) }                         (Eq.7)

The right hand side of ``(Eq.7)`` produces another value of ``a`` which
is ``S O``, as follows. In ``(Eq.7)``, distribute ``a == S n'`` we get:

::

   lt a (S(S(S(S(S O))))) =  a == O
                          |  fresh n' in
                       a == S n' &  n' == O
                             | a == S n' & fresh n'' in n' == S n'' & lt n'' (S(S(S O)))

                                                        (Eq.8)

Replace ``a == S n' &  n' == O`` by ``a == S O`` in ``(Eq.8)``, we get:

::

   lt a (S(S(S(S(S O))))) =  a == O
                          |  fresh n' in
                       a == S O
                             | a == S n' & fresh n'' in  n' == S n'' & lt n'' (S(S(S O)))

                                                                                (Eq.9)

In the right hand side of ``(Eq.9)`` move ``a == S O`` out of the scope
of the ``fresh n' in``, we have:

::

   lt a (S(S(S(S(S O))))) =  a == O
                          |  a == S O
                  |  fresh n' in
                     a == S n' & fresh n'' in n' == S n'' & lt n'' (S(S(S O)))

                                                                                (Eq.10)

From ``(Eq.1)`` to ``(Eq.10)`` are steps of substitution, unification
and simplification. Recursive calls are expanded and then reduced, and
the initial formula ``lt a (S(S(S(S(S O)))))`` is gradually unfolded so
that values of ``a`` are revealed one by one. Continue this way, the
last but one equation would be:

::

   lt a (S(S(S(S(S O))))) =  a == O
                          |  a == S O
                  |  a == S (S O)
                  |  a == S (S (S O))
                  |  a == S (S (S (S O)))
                  |  fresh n' in a == S n'
                  &  fresh n'' in n' == S n''
                  &  fresh n''' in n'' == S n'''
                  &  fresh n'''' in n''' == S n''''
                  &  fresh n''''' in n'''' == S n''''' & lt n''''' O
                                                                    (Eq.11)

Note that ``lt n''''' O`` expands to ``fresh n in O == S n & ...`` which
is false, therefore the last equation is:

::

   lt a (S(S(S(S(S O))))) =  a == O
                          |  a == S O
                  |  a == S (S O)
                  |  a == S (S (S O))
                  |  a == S (S (S (S O)))                               (Eq.12)

From ``(Eq.12)`` we read off the answers to the query.

The derivation from ``(Eq.1)`` to ``(Eq.12)``, combined with the
operational semantics of OCanren in terms of stream manipulation,
explains why we get the answer that ``a`` equals 0,1,2,3 or 4 from the
goal ``(G.1)``.

The reader may take an exercise to show that one plus one equals two by
simplifying the formula ``add (S O) (S O) c``.

(T.7) Modifying the Search Behaviour
------------------------------------

We compare two versions of the *simplify* relation, differing from each
other only by a swap of conjuncts.

Both versions share the logic that the simplest form of ``a/b`` is
``a'/b'`` where ``a'`` (``b'``) is ``a``\ (resp. ``b``) divided by the
greatest common divisor of ``a`` and ``b``, provided ``b`` is non-zero.
There is a short cut for the case where ``a`` is zero, then ``b'`` is
set to one directly.

The difference is that: - In one version we say, “``a`` (``b``) divided
by ``c`` equals ``a'`` (resp. ``b'``), and ``c`` is the gcd of ``a`` and
``b``.” - In the other version we say, “``c`` is the gcd of ``a`` and
``b``, and ``a`` (``b``) divided by ``c`` equals ``a'`` (resp. ``b'``).”

In OCanren:

.. code:: ocaml

   let simplify a b a' b' =
         ocanren {  fresh n in b == S n &
         { a == O & a' == O & b' == S O
         | fresh c, m in a == S m
                         & div a c a' O             (* div first, then gcd *)
                         & div b c b' O
                         & gcd a b c } };;

   let simplify' a b a' b' =
         ocanren {  fresh n in b == S n &
         { a == O & a' == O & b' == S O
         | fresh c, m in a == S m
                         & gcd a b c                (* gcd first, then div *)
                         & div a c a' O
                         & div b c b' O  } };;

The test file offers a `comparison <test.ml#L199>`__ of these two
versions over their forward and backward search behaviours. By *forward
search* we mean that given a ratio *a/b* find its simplest form, e.g.,
18/12 is simplified to 3/2. By *backward search* we mean given a ratio
in the simplest form, find its equal ratios, e.g., 3/2 could be
simplified from 6/4, 9/6, 12/8, etc. The test shows that both versions
work well for forward search, but when it comes to backward search,
``simplify`` returns answers quickly but ``simplify'`` took ages without
returning anything.

The ordering of the conjuncts, together with the state of the logic
varaibles and the search behaviour of the sub-relations, results in
apparently different operational meaning of the conjunctions in backward
search, as follows:

+------------+----------------+-----------------+---------------------+
| Ordering   | Operational    | State of        | Knowledge on        |
| of         | Meaning        | Variables       | Sub-relations       |
| Conjuncts  |                |                 |                     |
+============+================+=================+=====================+
| di         | Find ``a`` and | Before the      | This analysis       |
| v a c a’ O | ``c``\ such    | execution of    | requires knowledge  |
| & di       | that ``a``     | the first       | of the search       |
| v b c b’ O | divided by     | conjunct, both  | behaviour of        |
| &          | ``c`` equals   | ``a,c`` are     | ``div ar            |
|  gcd a b c | ``a'``         | unknowns. When  | g1 arg2 arg3 arg4`` |
|            | exactly. Then  | the second      | in the following    |
|            | find ``b``     | conjunct is to  | two cases: i. Both  |
|            | such that      | be executed,    | ``arg1, arg2`` are  |
|            | ``b`` divided  | ``c`` has       | unknowns, but       |
|            | by ``c``       | already been    | ``arg3, arg4`` are  |
|            | equals ``b'``  | found by the    | known. ii. Only     |
|            | exactly. Now   | first conjunct, | ``arg1`` is         |
|            | check that the | and only ``b``  | unknown, the other  |
|            | gcd of ``a``   | is the unknown. | three are known.    |
|            | and ``b`` is   | Right before    |                     |
|            | ``c``.         | the execution   |                     |
|            |                | of the thrid    |                     |
|            |                | conjunct, all   |                     |
|            |                | ``a,b,c`` have  |                     |
|            |                | been found so   |                     |
|            |                | only a check is |                     |
|            |                | due.            |                     |
+------------+----------------+-----------------+---------------------+
| gcd a b c  | Find three     | Before the      | This analysis       |
| & di       | unkno          | first conjunct  | requires knowledge  |
| v a c a’ O | wns\ ``a,b,c`` | is executed,    | of the search       |
| & di       | such that the  | all ``a,b,c``   | behaviour of        |
| v b c b’ O | relation       | are unknown,    | ``gcd`` when        |
|            | ``gcd a b c``  | but by the time | provided with three |
|            | holds, then    | the second and  | free logic          |
|            | check that     | third conjuncts | variables for its   |
|            | ``a`` (``b``)  | are to be       | three arguments.    |
|            | is exactly     | executed, the   |                     |
|            | dividable by   | variables       |                     |
|            | ``c`` with     | ``a,b,c`` are   |                     |
|            | quotient       | already         |                     |
|            | ``a'`` (resp.  | computed by the |                     |
|            | ``b'``).       | first conjunct, |                     |
|            |                | therefore the   |                     |
|            |                | last two        |                     |
|            |                | conjuncts       |                     |
|            |                | merely check    |                     |
|            |                | the result.     |                     |
+------------+----------------+-----------------+---------------------+

The relevant search behaviours of the sub-relations mentioned in the
table can be observed by running the test file or found in
`answers.txt <answers.txt>`__. For instance, to know the search
behaviour of ``div`` when only its first and second argument are
unknown, we can make the specific query:

.. code:: ocaml

   printf "\n What divided by what equals 3 with remainder 2 ? (give %d answers) \n\n" ans_no;
   ocrun2 ~n:ans_no (fun q r-> ocanren { div q r (S(S(S O))) (S(S O)) })

The answers are:

::

    What divided by what equals 3 with remainder 2 ? (give 20 answers)

   (11, 3)
   (14, 4)
   (17, 5)
   (20, 6)
   (23, 7)
   (26, 8)
   (29, 9)
   (32, 10)
   (35, 11)
   (38, 12)
   (41, 13)
   (44, 14)
   (47, 15)
   (50, 16)
   (53, 17)
   (56, 18)
   (59, 19)
   (62, 20)
   (65, 21)
   (68, 22)

We could see that the ``div`` relation is enumerating all possible
divisors in ascending order, starting with the least possible divisor
which is 3 (the divisor must be greater than the remainder 2), together
with the corresponding dividends.

In backward search, therefore, the ``simplify`` relation first finds a
``c``-multiple of ``a'`` for some ``c``, and then finds a ``c``-multiple
of ``b'`` for the same ``c``. Its check of the gcd relation as the last
step is starighforward if ``a'/b'`` is already in the simplest form.
Note that the programmer provides ``a'`` and ``b'`` so practically
``a'/b'`` does not have to be in the simplest form, in which case the
``gcd`` check would fail. This all sounds like logical manners to find
integral multiples of a ratio that is in the simplest form. However, the
way in which ``simplify'`` approaches the problem is firstly guessing an
arbitrary ratio together with the gcd of the numerator and the
denominator, and then it checks if the ratio happens to reduce to
``a'/b'``. This obviously has a bad chance to hit the target. That’s why
``simplify`` works better than ``simplify'`` for backward search, and
they only differ by a swap of conjuncts.

Note worthy is that the advantage of ``simplify`` over ``simplify'`` in
backward search is at the cost of some efficiency in forward search,
where ``simplify'`` smartly finds the gcd of the numerator and the
denominator first and then divides to get the result, but ``simplify``
enumerates through all divisors of ``a`` to find the one that is also a
divisor of ``b`` and the gcd of ``a,b`` — less efficient but still
acceptable for small numbers.

As an exercise, the reader could experiment with reordering the
conjuncts so that ``gcd`` is placed in between the two ``div``\ ’s. How
would forward and backward search be influenced? A second question: what
will happen and why, if we use ``simplify`` to find ``a`` and ``b``, but
give ``a'`` and ``b'`` as 4 and 2 respectively, i.e., a ratio not in the
simplest form?

(T.8) The Trick of Generate-and-test
------------------------------------

When using the ``gcd`` relation to answer the question: “What and what
have gcd 7 ?”, the distribution of `the answers <answers.txt#L432>`__
does not look balanced: the second number is 7 most of the time, while
the first number is growing. In comparison, `the
answers <answers.txt#L455>`__ given by the ``gcd'`` relation has a more
satisfactory distribution: the first number increases and for each
possible first number, all possible second numbers are enumerated before
the first number is further increased. The ``gcd'`` relation is defined
using the famous technique known as *generate-and-test*, which we
explain now.

Browsing the library `source <peano.ml#L81>`__ we could see that
``gcd'`` is defined in terms of ``gcd`` together with the addition
relation and the Peano number predicate ``isp`` (read “is P” or “is a
Peano Number”).

Provided a free variable as the argument, ``isp`` enumerates all Peano
numbers, in other words, we can obtain the following equation from the
definition of ``isp``, where the right hand side is an infinite formula:

::

   isp n = n == O | n == S O | n == S (S O) | n == S (S (S O)) | ...

Therefore, ``isp`` could be a Peano number *generator* .

Moreover, when the third argument of ``add`` is concrete but the first
and second argument are free variables, the ``add`` relation can find
all ways to break up the third argument into two addends.

The sequence of ``isp`` and ``add`` in the body of ``gcd'`` can then be
a Peano number pair generator, enumerating all possible pairs of Peano
numbers (in the same way Georg Cantor shows that the set of rational
numbers is enumerable). Now the way ``gcd'`` works is clear: it
enumerates through (i.e., *generates*) all possible pairs and then
*tests* which pairs have the gcd 7. Since the pairs are generated
systematically , the final answers are organized in the way we saw.

Another example of generate-and-test is the ``simplify a b a' b'``
relation. When ``a,b`` are given but ``a',b'`` are left unknown, the
`first ``div`` <peano.ml#L91>`__ generates all possible divisor-quotient
pairs for ``a``, and for each such pair the `second
``div`` <peano.ml#L92>`__ tests if the divisor also divides ``b`` and if
so generates the quotient. The sequence of two ``div``\ ’s then plays
the role of a generator of all common divisors of ``a,b`` together with
the corresponding pairs of numbers which are ``a,b`` divided by their
common divisors. The ```gcd`` sub-relation <peano.ml#L93>`__ finally
checks for the greatest common divisor, and the corresponding pair of
quotients is the answer for ``a',b'``.

In summary, generate-and-test is both a technique that the programmer
applies to solve certain problems (e.g., the ``gcd'`` case), and a usual
way in which relational programs search for answers even if the
programmer does not intentionally apply it (e.g., the ``simplify``
case).

(T.9) The Formula Parser
------------------------

In the library implementation and the test file, we often see formulae
enclosed by the ``ocanren{}`` quotation which takes care of, among
others, precedence and associativity of the logic connectives. We take a
look at the
`implementation <../../Installation/ocanren/camlp5/pa_ocanren.ml>`__ of
the ``ocanren{}`` quotation which we will call *the formula parser* in
the rest of this lesson. Our terminology follows `Camlp5 Reference
Manual <https://camlp5.github.io/doc/htmlc/>`__. We take a top-down
approach, starting with an overview of the structure of the parser, then
explain its individual parts.

The structure of the parser: an overview
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We describe the formula parser as the reader (a Camlp5 novice) sees it,
and then putting it in perspective, briefly explain how it works.

What we see
^^^^^^^^^^^

The first line loads the Camlp5 syntax extension kit ``pa_extend.cmo``
where ``pa_`` in the name stands for “parser”, and ``extend`` refers to
the syntactic category named “extend”, so that the name “pa_extend”
means “parser for the ‘extend’ syntactic category.”:

.. code:: ocaml

   #load "pa_extend.cmo";;

Loading the kit amounts to extending the OCaml syntactic category
`expression <https://ocaml.org/releases/4.11/htmlman/expr.html>`__ with
several sub-categories one of which is named *extend*:

.. code:: ebnf

   expr = ... | extend ;
   extend = "EXTEND", extend-body, "END" ;

An expression that belongs to the category “extend” would be called an
*EXTEND statement*.

Our formula parser has only
`one <../../Installation/ocanren/camlp5/pa_ocanren.ml#L168>`__ EXTEND
statement, whose extend-body starts with a `global
indicator <../../Installation/ocanren/camlp5/pa_ocanren.ml#L169>`__
followed by a semicolon separated list of *entries* (whose names are,
exhaustively,
```long_ident`` <../../Installation/ocanren/camlp5/pa_ocanren.ml#L171>`__,
```expr`` <../../Installation/ocanren/camlp5/pa_ocanren.ml#L186>`__,
```ocanren_embedding`` <../../Installation/ocanren/camlp5/pa_ocanren.ml#L222>`__,
```ocanren_expr`` <../../Installation/ocanren/camlp5/pa_ocanren.ml#L226>`__,
```ocanren_term`` <../../Installation/ocanren/camlp5/pa_ocanren.ml#L255>`__,
```ocanren_term'`` <../../Installation/ocanren/camlp5/pa_ocanren.ml#L259>`__
and
```ctyp`` <../../Installation/ocanren/camlp5/pa_ocanren.ml#L290>`__). An
*entry* is a (vertical bar separated) list of *levels* (with a pair of
enclosing square brackets); a *level* is a (vertical bar separated) list
of *rules* (with a pair of enclosing square brackets); a (non-empty)
*rule* is a (semicolon separated) list of “psymbols” (collectively
acting as a pattern) followed by an optional semantic action that
produces an abstract syntax tree (or AST, of any string that matches the
pattern specified by the list of psymbols). The details on the syntax
and semantics of the “extend” category can be found in the `Extensible
Grammars <https://camlp5.github.io/doc/htmlc/grammars.html#a:Syntax-of-the-EXTEND-statement>`__
section of the Camlp5 Manual.

Besides the EXTEND statement our formula parser has some auxiliary
functions such as
```decapitalize`` <../../Installation/ocanren/camlp5/pa_ocanren.ml#L46>`__,
```ctor`` <../../Installation/ocanren/camlp5/pa_ocanren.ml#L49>`__ and
```fix_term`` <../../Installation/ocanren/camlp5/pa_ocanren.ml#L61>`__
etc.

How it works
^^^^^^^^^^^^

The syntax extension kit ``pa_extend.cmo`` is fundamental for the
cascade of extensions described below. The entries ``expr`` and ``ctyp``
origin from the module Pcaml that is the core of Camlp5 and is
`opened <../../Installation/ocanren/camlp5/pa_ocanren.ml#L37>`__ by the
formula parser. Pcaml initializes the (empty) grammar entries ``expr``
and ``ctyp``. The standard OCaml parsing kit of Camlp5 then defines them
by means of an EXTEND statement and accordng to the standard syntax of
OCaml. Our EXTEND statement further extends these global entries with
locally defined entries — entries other than ``expr`` and ``ctyp`` in
our EXTEND statement are locally defined, such as ``ocanren_embedding``,
``ocanren_expr`` and ``ocanren_term`` etc. The following table
summarizes the stages of extension, providing links to copies of
relevant files from either OCanren source or Camlp5 source, together
with their documentations.

+------------------------+---------+----------------------------------+
| Stages of Extension    | Happens | Documentation                    |
|                        | in file |                                  |
+========================+=========+==================================+
| Stage 1.               | `       | `The Pcaml                       |
| Initialization         | Pcaml < | module <https://camlp5.gi        |
|                        | camlp5_ | thub.io/doc/htmlc/pcaml.html>`__ |
|                        | src_ref |                                  |
|                        | /pcaml. |                                  |
|                        | ml>`__: |                                  |
|                        | `       |                                  |
|                        | ``expr` |                                  |
|                        | ` <caml |                                  |
|                        | p5_src_ |                                  |
|                        | ref/pca |                                  |
|                        | ml.ml#L |                                  |
|                        | 53>`__, |                                  |
|                        | ```ctyp |                                  |
|                        | `` <cam |                                  |
|                        | lp5_src |                                  |
|                        | _ref/pc |                                  |
|                        | aml.ml# |                                  |
|                        | L56>`__ |                                  |
+------------------------+---------+----------------------------------+
| Stage 2. Parsing Kit   | `p      | `Commands and                    |
| for Standard OCaml     | a_o.ml  | Files <https://camlp5.githu      |
|                        | <camlp5 | b.io/doc/htmlc/commands.html>`__ |
|                        | _src_re |                                  |
|                        | f/pa_o. |                                  |
|                        | ml>`__: |                                  |
|                        | `       |                                  |
|                        | ``expr` |                                  |
|                        | ` <caml |                                  |
|                        | p5_src_ |                                  |
|                        | ref/pa_ |                                  |
|                        | o.ml#L5 |                                  |
|                        | 56>`__, |                                  |
|                        | ```ctyp |                                  |
|                        | `` <cam |                                  |
|                        | lp5_src |                                  |
|                        | _ref/pa |                                  |
|                        | _o.ml#L |                                  |
|                        | 950>`__ |                                  |
+------------------------+---------+----------------------------------+
| Stage 3. OCanren       | `pa_o   | This document                    |
| Formula Parser         | canren. |                                  |
|                        | ml <../ |                                  |
|                        | ../Inst |                                  |
|                        | allatio |                                  |
|                        | n/ocanr |                                  |
|                        | en/caml |                                  |
|                        | p5/pa_o |                                  |
|                        | canren. |                                  |
|                        | ml>`__: |                                  |
|                        | ```ex   |                                  |
|                        | pr`` <. |                                  |
|                        | ./../In |                                  |
|                        | stallat |                                  |
|                        | ion/oca |                                  |
|                        | nren/ca |                                  |
|                        | mlp5/pa |                                  |
|                        | _ocanre |                                  |
|                        | n.ml#L1 |                                  |
|                        | 86>`__, |                                  |
|                        | ```c    |                                  |
|                        | typ`` < |                                  |
|                        | ../../I |                                  |
|                        | nstalla |                                  |
|                        | tion/oc |                                  |
|                        | anren/c |                                  |
|                        | amlp5/p |                                  |
|                        | a_ocanr |                                  |
|                        | en.ml#L |                                  |
|                        | 290>`__ |                                  |
+------------------------+---------+----------------------------------+

As a preprocessing tool, Camlp5 defines its own parser ``pa_o.ml`` for
standard OCaml, so that any standard OCaml code can be converted by it
into an AST recongnizable by the `OCaml
compiler <https://ocaml.org/releases/4.11/htmlman/comp.html>`__. Is
``pa_o.ml`` a redundant piece of work for we can just use the OCaml
compiler to build the AST ? Not exactly, because besides ``pa_o.ml``,
Camlp5 also provides EXTEND statments so that syntactic categories
defined in ``pa_o.ml`` can be extended. The result is that using the
combination of ``pa_ocanren.ml`` and ``pa_o.ml`` we can convert code
that is not wholly in OCaml into a purely OCaml AST.

Conclusion
^^^^^^^^^^

The OCanren formula parser has the EXTEND statement as its core, which
consists of a list of entries, notably the global entries ``expr``\ and
``ctyp`` that extend the corresponding predefined entries that conform
to standard OCaml. Such extension is characterized by the locally
defined entries such as ``ocanren_embedding``.

We will next focus on the extension of ``expr`` and leave ``ctyp``
`aside <./ctyp>`__. As far as the semantics is concerned entries are
parsers for syntactic categories. From now on we use the words “entry”
and “parser” interchangeably.

The global entry: ``expr``
~~~~~~~~~~~~~~~~~~~~~~~~~~

This is the major entry of the OCanren formula parser, which starts
like:

.. code:: ocaml

   expr: LEVEL "expr1" [ ...

where we see the entry name *expr* and the position ``LEVEL "expr1"``.
We now use OCanren-``expr`` to refer to the ``expr`` entry in the
OCanren formula parser, and OCaml-``expr`` to refer to the predefined
entry ``expr`` in the Camlp5 parsing kit for standard OCaml.
OCanren-``expr`` extends OCaml-``expr`` in the position
``LEVEL "expr1"``: the first level of the OCanren-``expr`` is merged
with the `level named “expr1” <camlp5_src_ref/pa_o.ml#L563>`__ of the
OCaml-``expr``, i.e., their rules are put together and grouped as a
single level named “expr1”; other levels from OCanren-``expr`` are
inserted into OCaml-``expr`` as new levels, right below the extended
“expr1” level. There are three levels in the OCanren-``expr``, the third
of which is:

.. code:: ocaml

   [ e=ocanren_embedding -> e ]

This third level of OCanren-``expr`` is inserted as a new level in
OCaml-``expr``, and the entry ``ocanren_embedding`` directly corresponds
to the ``ocanren{}`` quotations we see in the library implementation, so
that we can mix ``ocanren{}`` quotations with standard OCaml
expressions, and Camlp5 will take care to convert such mixture into
standard OCaml AST. We now explain the local entry
``ocanren_embedding``.

Local entries I: ``ocanren_embedding`` and ``ocanren_expr``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The entry ``ocanren_embedding`` directly corresponds to the
``ocanren{}`` quotations we see in the library implementation, and it
further calls the entry ``ocanren_expr`` to parse the content between
the braces:

.. code:: ocaml

   ocanren_embedding: [[ "ocanren"; "{"; e=ocanren_expr; "}" -> e ]];

The ``ocanren_expr`` entry has four levels which strongly reminds us of
the recursive definition of a formula, i.e, a formula is either atomic,
or a conjunction/ disjunction of two formulae, or an existential
quantification over a formula, or an explicitly delimited formula (with
braces). - The `first
level <../../Installation/ocanren/camlp5/pa_ocanren.ml#L227>`__ parses a
disjunction:
``ocaml     "top" RIGHTA [ l=SELF; "|"; r=SELF -> <:expr< OCanren.disj $l$ $r$ >> ]``
- The `second
level <../../Installation/ocanren/camlp5/pa_ocanren.ml#L228>`__ parses a
conjunction:
``ocaml    RIGHTA [ l=SELF; "&"; r=SELF -> <:expr< OCanren.conj $l$ $r$ >> ]``
- The `third
level <../../Installation/ocanren/camlp5/pa_ocanren.ml#L229>`__ parses a
fresh variable introduction (i.e., existential quantification):
``ocaml    [ "fresh"; vars=LIST1 LIDENT SEP ","; "in"; b=ocanren_expr LEVEL "top" ->        List.fold_right          (fun x b -> let p = <:patt< $lid:x$ >> in <:expr< OCanren.call_fresh ( fun $p$ -> $b$ ) >>) vars b  ]``
- The `fourth
level <../../Installation/ocanren/camlp5/pa_ocanren.ml#L238>`__ parses
atomic, named and grouped formulae (and else):
``ocaml    "primary" [       | l=ocanren_term; "==" ; r=ocanren_term         -> <:expr< OCanren.unify $l$ $r$ >>       | l=ocanren_term; "=/="; r=ocanren_term         -> <:expr< OCanren.diseq $l$ $r$ >>       | x=ocanren_term                                -> x       | "{"; e=ocanren_expr; "}"                      -> e       (* other rules omitted *)  ]``

The order of the levels determines the precedence of the logic
connectives: the parser first sees if the formula is a disjunction at
the top level, if not, sees if it is conjunction, and so on, implying
that disjunction has the least precedence, above which is conjunction,
then existential quantification, and finally syntactic equality,
disequality and braced groups (among others) enjoy the highest
precedence. We can justly call a level: a “precedence level”.

The first and second level also have the associativity indicator
``RIGHTA``, requiring that the conjunction and disjunction connectives
associate to the right.

The third level refers back to the first level (named “top”) when
parsing the ``<body>`` part of a formula of the form
``fresh <vars> in <body>``, implying that the scope of ``fresh`` extends
to the right as far as possible.

Quotations and antiquotations
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In every rule above we could see at least one
`quotation <https://camlp5.github.io/doc/htmlc/quot.html>`__:

.. code:: ebnf

   quotation = "<:", quotation name, "<", quotation body, ">>"

Within a quotation body we may see an
`antiquotation <https://camlp5.github.io/doc/htmlc/quot.html#a:Antiquotations>`__:

.. code:: ebnf

   antiquotation = "$", antiquotation body, "$"

If antiquotations are not allowed, then a quotation body is any
expression in the `revised
syntax <https://camlp5.github.io/doc/htmlc/revsynt.html>`__ of OCaml. At
parse time a quotation is expanded by the
(`loaded <../../Installation/ocanren/camlp5/pa_ocanren.ml#L35>`__ and
`predefined <https://camlp5.github.io/doc/htmlc/ast_strict.html#a:Nodes-and-Quotations>`__)
quotation expander ``q_MLast.cmo`` into an AST of the quotation body. An
antiquotaion body is usually a pattern variable bound to some other AST
which is inserted into the the quotation body’s AST.

Local entries II: ``ocanren_term`` and ``ocanren_term'``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The values that we write in an ``ocanren{}`` quotation, such as
``"this is a string"``, ``'c'`` (a single character), ``true`` (a
boolean value), ``S (S O)`` (a constructor application), ``(O, S O)`` (a
tuple), ``15`` (an integer), ``[1;2;3]`` (a list) and ``false :: []``
(amending a list) etc., are converted into the injected level from the
ground level where they seem to be. For example, the occurrence of
``S (S O)`` in the expression below is transformed into
``s (s (o ()))``:

.. code:: ocaml

   ocanren { fresh x in S (S O) == x };;

Such conversion bridges the gap between the programmer’s intuition of
writing OCaml values and OCanren’s internal representation of the same
values, Inspecting the entries ``ocanren_term``, ``ocanren_term'`` and
their auxiliary functions help us know precisely how the conversion is
performed.

Below is the definition of the entry ``ocanren_term``:

.. code:: ocaml

   ocanren_term: [[ t=ocanren_term' -> fix_term t ]];

where the ``ocanren_term'`` parser is called immediately to process
expressions like ``S (S O)`` and the intermediate result (an AST) is
bound to the pattern variable ``t`` and then passed to the auxiliary
function ``fix_term``. The AST returned by ``fix_term`` is returned by
the parser ``ocanren_term``.

The ``ocanren_term'`` parser has four levels, namely: 1.
`“app” <../../Installation/ocanren/camlp5/pa_ocanren.ml#L260>`__, for
applications.
``ocaml    "app"  LEFTA  [ l=SELF; r=SELF -> <:expr< $l$ $r$ >> ]``
Applications are treated as being left associative as indicated by
``LEFTA``. This level not yet converts constructor applications into
injection function applications. Instead it simply builds the AST of the
application in a straightforward manner, not distinguishing a
constructor application from a function application. 1.
`“list” <../../Installation/ocanren/camlp5/pa_ocanren.ml#L261>`__ , for
non-empty lists with ``::`` as the top level constructor.
``ocaml    "list" RIGHTA [ l=SELF; "::"; r=SELF -> <:expr< OCanren.Std.List.cons $l$ $r$ >> ]``
The constructor ``::`` is replaced by the OCanren standard library
function ```cons`` <../../Installation/ocanren/src/std/LList.mli#L47>`__
which is the injection function for the constructor
```OCanren.Std.List.Cons`` <../../Installation/ocanren/src/std/LList.mli#L27>`__.
1. `“primary” <../../Installation/ocanren/camlp5/pa_ocanren.ml#L262>`__,
which has rules for: -
`anti-quotations <../../Installation/ocanren/camlp5/pa_ocanren.ml#L262>`__
``ocaml      "!"; "("; e=expr; ")" -> e`` So that the ``ocanren{}``
quotation would take any ``<value>`` from ``!(<value>)`` as is without
further processing. In other words, the ``<value>`` will be parsed using
the entry ``expr``. -
`integers <../../Installation/ocanren/camlp5/pa_ocanren.ml#L263>`__
``ocaml      c=INT -> let n = <:expr< $int:c$ >> in <:expr< OCanren.Std.nat $n$ >>``
Thus, occurrences of integers like ``15`` within the ``ocanren{}``
quotation would be converted to values of the Peano number type that is
provided by the OCanren standard library
`OCanren.Std.Nat <../../Installation/ocanren/src/std/LNat.mli>`__. -
`characters <../../Installation/ocanren/camlp5/pa_ocanren.ml#L266>`__
and `strings <../../Installation/ocanren/camlp5/pa_ocanren.ml#L269>`__
``ocaml        c=CHAR   -> let s = <:expr< $chr:c$ >> in <:expr< OCanren.inj (OCanren.lift $s$) >>      | s=STRING -> let s = <:expr< $str:s$ >> in <:expr< OCanren.inj (OCanren.lift $s$) >>``
Characters and strings are injected using the primary injection function
``!!`` (see its
`signature <../../Installation/ocanren/src/core/Logic.mli#L57>`__ and
`implementation <../../Installation/ocanren/src/core/Logic.ml#L65>`__).
- `booleans <../../Installation/ocanren/camlp5/pa_ocanren.ml#L272>`__
``ocaml        "true"   -> <:expr< OCanren.Std.Bool.truo >>      | "false"  -> <:expr< OCanren.Std.Bool.falso >>``
Boolean values are converted into the corresponding injected values from
the OCanren standard library
`LBool <../../Installation/ocanren/src/std/LBool.mli#L45>`__. - `lists
delimited by ``[]`` and
``;`` <../../Installation/ocanren/camlp5/pa_ocanren.ml#L274>`__
``ocaml      "["; ts=LIST0 ocanren_term' SEP ";"; "]" ->       ( match ts with       | [] -> <:expr< OCanren.Std.nil () >>       | _  -> List.fold_right (fun x l -> <:expr< OCanren.Std.List.cons $x$ $l$ >> )                               ts <:expr< OCanren.Std.nil () >> )``
The entry ``ocanren_term'`` is recursively called to process the list
members and the injection functions for list constructors are applied. -
`operators <../../Installation/ocanren/camlp5/pa_ocanren.ml#L279>`__
(which are not qualified)
``ocaml       "("; op=operator_rparen -> <:expr< $lid:op$ >>`` Operators
are specified by the auxiliary function
```is_operator`` <../../Installation/ocanren/camlp5/pa_ocanren.ml#L92>`__
and extracted by another auxiliary function
```operator_rparen`` <../../Installation/ocanren/camlp5/pa_ocanren.ml#L104>`__
(the name of which reads “operator right parenthesis”). - (possibly
empty) `tuples <../../Installation/ocanren/camlp5/pa_ocanren.ml#L280>`__
``ocaml      "("; ts=LIST0 ocanren_term' SEP ","; ")" ->       (match ts with        | []  -> <:expr< OCanren.inj (OCanren.lift ()) >>        | [t] -> t        | _   -> <:expr< ( $list:ts$ ) >> )``
There is a recursive call of the entry itself to process members of the
tuple, and then the AST of the tuple is built. 1. The level for long
identifiers. ``ocaml    [ long_ident ]`` This level calls the entry
```long_ident`` <../../Installation/ocanren/camlp5/pa_ocanren.ml#L171>`__
to build AST’s of (possibly qualified) upper / lower case identifiers
and operators which are taken as is.

Therefore, given ``S (S O)`` the ``ocanren_term'`` parser would return a
straightforward translation into an AST. The interesting thing is done
by ``fix_term`` and its helper ``ctor`` (read “C-tour”). The latter
tests the input: if it is a (possibly qualified) uppercase identifier
then sets the initial letter to lowercase and wraps the whole thing by
``Some``, e.g., ``Mod1.Mod2.ABC`` becomes (roughly)
``Some Mod1.Mod2.aBC``; if the input is not a (qualified) uppercase
identifier then returns ``None``:

.. code:: ocaml

   let rec ctor e =
     let loc = MLast.loc_of_expr e in
     match e with
     | <:expr< $uid:u$ >>   -> Some (<:expr< $lid:decapitalize u$ >>)
     | <:expr< $m$ . $e$ >> -> (match ctor e with Some e -> Some (<:expr< $m$ . $e$ >>) | _ -> None)
     | _                    -> None

The
```fix_term`` <../../Installation/ocanren/camlp5/pa_ocanren.ml#L61>`__
function then recurses down the structure of applications to
systematically replace uppercase identifiers with lowercase identifiers
produced by ``ctor``. After a constant constructor is changed to
lowercase, it is provided with the unit value ``()`` as the argument,
e.g., ``O`` becomes ``o ()``. A non-constant constructor is not only set
to lowercase, but also has its argument list transformed, e.g.,
``Cons(a,b)`` becomes (roughly) ``cons a b``. Tuples are also replaced
by their OCanren standard library counterpart — `logic
tuples <../../Installation/ocanren/src/std/LPair.mli>`__.

These lowercase identifiers converted from constructors are supposed to
be injection functions, which must be defined by the programmer
somewhere in the program, otherwise there would be some compile-time
error like “unbound identifier”. This explains why the injection
function names are always differ from the corresponding constructor
names by one letter: the initial letter.

(T.10) Building a Library
-------------------------

Being essentially an OCaml library, an OCanren library shall have its
interface ``.mli`` and implementation ``.ml``. The interface typically
contains type definitions (at the four levels), auxiliaries (such as
injection functions, reifiers, etc.) and the relations. Note also the
``@type`` syntax in the interface.

Relations shall be systematically tested. This means all possible
combinations of unknown arguments shall be queried wrt. each relation.
The benefits of such systematic test includes reducing surprises when
running the programs, and helping with debugging “bigger” relations that
are defined using smaller relations. For example, understanding the
search behaviour of ``simplify`` requires knowledge of search behaviours
of ``div`` and ``gcd`` and in turn those of ``lt``, ``lte`` and ``add``.
