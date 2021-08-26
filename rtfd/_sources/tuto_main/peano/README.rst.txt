A Library for Peano Arithmetic
==============================

We hope the reader will learn the following techniques   from this lesson:

- `Advanced injection functions <#advanced-injection-functions>`__ Defining injection functions for value constructors of variant types, using the Fmap family of module functors ``Fmap``, ``Fmap2``, ``Fmap3``, etc., which are provided by the module `Logic <../../Installation/ocanren/src/core/Logic.mli>`__.
- `Reification and Reifiers <#reification-and-reifiers>`__ Defining reifiers to convert data from the injected level to the logic level, again with help from the Fmap family of module functors.
- `Overwriting the show Function <#overwriting-the-show-function>`__ Overwriting, or redefining the “show” function for values of a logic type, to allow for more concise and human readable printing of them.
- `Relations on Peano Numbers <#relations-on-peano-numbers>`__ Defining (possibly recursive) relations, e.g., comparison, addition and division on Peano numbers.
- `Scrutinizing Relations <#scrutinizing-relations>`__ Making queries to relations using combinations of unknown arguments.
- `Analyzing the search behaviour <#analyzing-the-search-behaviour>`__ Analyzing why a query returns certain answers.
- `Modifying the search behaviour <#modifying-the-search-behaviour>`__ Reordering the conjuncts within the body of a relation definition to modify the way in which the relation searches for answers in a given query.
- `The trick of generate and test <#the-trick-of-generate-and-test>`__ Programming a relation so that answers to certain queries are found by brute-force.
- `The formula parser <#the-formula-parser>`__ Observing that the implementation of the ``ocanren {}`` quotation takes care of the precedence, associativity and scope of the logic connectives, and replaces constructors of variant types by injection function names, and primitive values by their injected versions.
- `Building a library <#building-a-library>`__ Writing and testing a library in OCanren.

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

Advanced Injection Functions
----------------------------

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
respectively to the value constructors ``O`` and ``S`` :

.. code:: ocaml

   module Ty = struct
     @type 'a t = O | S of 'a with show, gmap
     let fmap = fun f d -> GT.gmap(t) f d
   end;;

   include Ty
   module F = Fmap(Ty)

   let o () = inj @@ F.distrib O
   let s n = inj @@ F.distrib (S n)


The general workflow of defining advanced injection functions is as
follows:

1. We begin with a variant type whose type constructor ``t`` has one or
   more type parameters. This is always an OCanren abstract type, such
   as the abstract Peano number type or the abstract list type.
2. We count the number of type parameters of the type constructor in
   order to choose the suitable module functor from the Fmap family: for
   one type parameter, use ``Fmap`` ; for two type parameters, use
   ``Fmap2``; three type parameters, ``Fmap3`` and so on.
3. We request the ``gmap`` plugin for the type constructor, and use it
   to define a function named ``fmap`` simply by renaming.
4. We put the definitions of the type constructor ``t`` and the ``fmap`` function in one module, and suppy that module as a parameter to the chosen Fmap family module functor. The result is a module ``F`` with
   three functions one of which is ``distrib``, the distribution
   function.
5. For each value constructor of the type ``t``, we define a function
   whose name is the same as the value constructor except that the
   initial letter is set to lower case. For example, ``Cons``, ``S`` and
   ``NUL`` become respectively ``cons``, ``s`` and ``nUL``.


- For each value constructor ``Constr0`` of no argument, define:

  .. code:: ocaml

      let constr0 = fun () -> Logic.inj @@ F.distrib Constr0

- For each value constructor ``Constr1`` of one argument, define:

   .. code:: ocaml

      let constr1 = fun x -> Logic.inj @@ F.distrib (Constr1 x)

- For each value constructor ``Constru`` of *u* (> 1) arguments,  define:

  .. code:: ocaml

      let constru = fun x1 ... xu -> Logic.inj @@ F.distrib @@ Constru (x1, ..., xu)


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

Reification and Reifiers
------------------------

Say we have a logic variable ``x`` and a substitution
``[(x, Lam(z,y)); (y, App(a,b))]`` that associates ``x`` with the term
``Lam(z,y)`` and ``y`` with ``App(a,b)`` where ``y, z`` are also logic
variables. We would like to know what ``x`` is with respect to the
substitution. It is straightforward to replace ``x`` by ``Lam(z,y)`` but
since ``y`` is associated with ``App(a,b)`` we can further replace ``y``
in ``Lam(z,y)``, and finally we get the term ``Lam(z,App(a,b))`` .
Although there is still an unbound part ``z``, we have no further
information about how ``z`` might be instantiated, so we leave it there.
What we have done is called *reification* of the logic variable ``x`` :
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

Overwriting the *show* Function
-------------------------------

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
``show`` : other plugins (or methods) keep their default meanings but
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

Some additional remarks on the last point: the ``@type``
definition of a
type constructor ``typeconstr-name`` generates a record value also named
``typeconstr-name`` of the type ``GT.t``. This could be viewed by adding
the ``-i`` option as indicated in the `Makefile <Makefile#L10>`__:

::

   BFLAGS = -rectypes -g -i

See also the `GT
source <https://github.com/JetBrains-Research/GT/blob/039193385e6cb1a67bc8a9d00c662d9d1dc5478b/src/GT.ml4#L37>`__.

Relations on Peano Numbers
--------------------------

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
              a == S n & c == S m & add n b m}

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
                  & lt n' n }}

It says that ``a`` is less than ``b`` if there exist ``n``, such that
``b`` equals ``S n``, and either ``a`` equals ``O`` or there exist
``n'`` such that ``a`` equals ``S n'`` and ``n'`` is less than ``n``.

Other relations in the library shall be read in this way, and they are
all written with the declarative reading in mind. The reader is
encouraged to write a relation for subtraction: ``sub a b c`` iff
``a - b = c``, or, put in another way: iff ``b`` is ``O`` and ``a`` is
``c``, or ``b`` is ``S n`` and ``a`` is ``S n'`` and ``sub n' n c``.

Scrutinizing Relations
----------------------

Taking the “less than” relation as an example, we can ask questions
like:

   - Is zero less than one ? Is one less than two ? Is one less than zero ? Is two less than one?
   - What is less than five ? Five is less than what ?
   - What is less than what ?

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

.. code:: ocaml

   fun q -> ocanren { lt O (S O) & lt (S O) (S(S O)) }

asks about what is ``q`` so that zero is less than one and one is less
than two, and the answer is just a free variable ``n`` meaning that ``q`` could be any
number and the relation always holds between the given numbers. The
similar goal:

.. code:: ocaml

   fun q -> ocanren { lt (S O) O | lt (S(S O)) (S O) }

asks about what is ``q`` so that one is less than zero or two is less
than one. There is no answer, meaning that there is no ``q`` to make the
relation hold between the given numbers.

The goal below asks what is less than five:

.. code:: ocaml

   fun q -> ocanren { lt q (S(S(S(S(S O))))) }

For this goal the answers ``0,1,2,3,4`` are found, which is quite
satisfactory.

The relations ``lte, add, div, gcd`` are also questioned systematically
in the test file.

Note that the addition relation can perform subtraction, and the
division relation can do multiplication. For instance, the goal below
asks “What adds 4 equals to 7 ?” and whose answer is “3”:

.. code:: ocaml

   fun q -> ocanren { add q (S(S(S(S O)))) (S(S(S(S(S(S(S O))))))) }

This amounts to performing the subtraction ``7 - 4``. The next goal asks
“What divided by 5 equals 3 with remainder 0 ?” and the answer is “15”:

.. code:: ocaml

   fun q -> ocanren { div q (S(S(S(S(S O))))) (S(S(S O))) O }

It amounts to the multiplication ``3 * 5``.

Analyzing the Search Behaviour
------------------------------

When asking the ``lt`` relation “what is less than 5” using the goal:

.. code:: ocaml

   fun q -> ocanren { lt q (S(S(S(S(S O))))) }                                  (G.1)

OCanren returns 0,1,2,3,4. Let’s see why. It really is a matter of
definition: we defined ``lt a b`` to be a certain formula ``(Eq.1)`` and
now we substitute 5 for ``b`` in the formula ``lt a b`` followed by
several steps of simplification then we get a formula ``(Eq.12)`` that
literally says ``a`` shall be 0, 1, 2, 3 or 4. Below are the details.

We reproduce the definition of ``lt`` in the followinig simplified form:

.. code:: ocaml

   lt a b = fresh n in b == S n
          & { a == O | fresh n' in a == S n' & lt n' n }
                                                                                (Eq.1)

Now replace ``b`` by ``(S(S(S(S(S O)))))`` in ``(Eq.1)``, we get:

.. code:: ocaml

   lt a (S(S(S(S(S O))))) = fresh n in (S(S(S(S(S O))))) == S n
                            & { a == O | fresh n' in a == S n' & lt n' n }
                                                                    (Eq.2)

Replace ``(S(S(S(S(S O))))) == S n`` by ``(S(S(S(S O)))) == n`` in
``(Eq.2)``, we get:

.. code:: ocaml

   lt a (S(S(S(S(S O))))) = fresh n in (S(S(S(S O)))) == n
                            & { a == O | fresh n' in a == S n' & lt n' n }
                                                                    (Eq.3)

In ``(Eq.3)``, remove ``fresh n in (S(S(S(S O)))) == n``, then replace
all free occurences of ``n`` by ``(S(S(S(S O))))``. The top level ``&``
and the braces are no longer needed, so also being removed. We get:

.. code:: ocaml

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
                        & lt n'' (S(S(S O)))                      (Eq.6)

Now in ``(Eq.4)`` replace ``lt n' (S(S(S(S O))))`` by the right hand
side of ``(Eq.6)``:

.. code:: ocaml

   lt a (S(S(S(S(S O))))) = a == O
                          | fresh n' in a == S n'
                          & { n' == O
                            | fresh n'' in n' == S n''
                              & lt n'' (S(S(S O))) }           (Eq.7)

The right hand side of ``(Eq.7)``
produces another value of ``a`` which
is ``S O``, as follows. In ``(Eq.7)``, distribute ``a == S n'`` we get:

.. code:: ocaml

   lt a (S(S(S(S(S O))))) = a == O
                          | fresh n' in
                              a == S n' &  n' == O
                            | a == S n' & fresh n'' in n' == S n'' & lt n'' (S(S(S O)))

                                                        (Eq.8)

Replace ``a == S n' &  n' == O`` by ``a == S O`` in ``(Eq.8)``, we get:

.. code:: ocaml

   lt a (S(S(S(S(S O))))) =  a == O
                          |  fresh n' in
                               a == S O
                             | a == S n' & fresh n'' in  n' == S n'' & lt n'' (S(S(S O)))

                                                                                (Eq.9)

In the right hand side of ``(Eq.9)`` move ``a == S O`` out of the scope
of the ``fresh n' in``, we have:

.. code:: ocaml

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

.. code:: ocaml

   lt a (S(S(S(S(S O)))))  =  a == O
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

.. code:: ocaml

   lt a (S(S(S(S(S O)))))  =  a == O
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

Modifying the Search Behaviour
------------------------------

We compare two versions of the *simplify* relation, differing from each
other only by a swap of conjuncts.

Both versions share the logic that the simplest form of ``a/b`` is
``a'/b'`` where ``a'`` (``b'``) is ``a`` (resp. ``b``) divided by the
greatest common divisor of ``a`` and ``b``, provided ``b`` is non-zero.
There is a short cut for the case where ``a`` is zero, then ``b'`` is
set to one directly.

The difference is that:

- In one version we say, “`a` (`b`) divided by `c` equals `a'` (resp. `b'`), and `c` is the gcd of `a` and `b`.”
- In the other version we say, “`c` is the gcd of `a` and `b`, and `a` (`b`) divided by `c` equals `a\'` (resp. `b\'`).”

In OCanren:

.. code:: ocaml

   let simplify a b a' b' =
     ocanren {  fresh n in
       b == S n &
       { a == O & a' == O & b' == S O
       | fresh c, m in
           a == S m
         & div a c a' O             (* div first, then gcd *)
         & div b c b' O
         & gcd a b c } }

   let simplify' a b a' b' =
     ocanren { fresh n in
        b == S n &
        { a == O & a' == O & b' == S O
        | fresh c, m in
            a == S m
          & gcd a b c                (* gcd first, then div *)
          & div a c a' O
          & div b c b' O  } }

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

1. *Variant 1*

  .. code::

      div a c a’ O &
      div b c b’ O &
      gcd a b c

  Find `a` and `c` such  that ``a`` divided by `c` equals `a\'` exactly. Then find ``b`` such that  ``b`` divided by `c`
  equals `b\'` exactly. Now check that the gcd of `a` and `b` is `c`.

  Before the execution of the first conjunct, both `a` and `c` are unknowns. When the second conjunct is to be executed, `c` has already been found by the first conjunct, and only `b` is the unknown. Right before the execution of the thrid conjunct, all `a,b,c` have been found  so only a check is due.

  This analysis requires knowledge of the search behaviour of `div arg1 arg2 arg3 arg4` in the following two cases:

  * Both `arg1, arg2` are unknowns, but `arg3, arg4` are known.
  * Only `arg1` is unknown, the other three are known.

1. *Variant 2*

  .. code::

      gcd a b c &
      div a c a’ O &
      div b c b’ O


  Find three unknowns`a,b,c` such  that the relation `gcd a b c` holds, then check that `a` (`b`) is exactly dividable by  `c` with quotient `a'` (resp. `b'`).

  Before the first conjunct is executed, all `a,b,c` are  unknown, but by the time the second and third conjuncts are to be executed, the variables `a,b,c` are already computed by the first conjunct, therefore the last two conjuncts merely check the result.

  This analysis requires knowledge of the search behaviour of `gcd` when provided  with three free logic variables for its three arguments.


The relevant search behaviours of the sub-relations mentioned in the
table can be observed by running the test file or found in
`answers.txt <answers.txt>`__. For instance, to know the search
behaviour of ``div`` when only its first and second argument are
unknown, we can make the specific query:

.. code:: ocaml

   printf "\n What divided by what equals 3 with remainder 2 ? (give %d answers) \n\n" ans_no;
   ocrun2 ~n:ans_no (fun q r -> ocanren { div q r (S(S(S O))) (S(S O)) })

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
``c``-multiple of ``a'`` for some ``c``, and then finds a
``c`` -multiple
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
conjuncts so that ``gcd`` is placed in between the two ``div`` ’s.
How
would forward and backward search be influenced? A second question: what
will happen and why, if we use ``simplify`` to find ``a`` and ``b``, but
give ``a'`` and ``b'`` as 4 and 2 respectively, i.e., a ratio not in the
simplest form?

The Trick of Generate-and-test
------------------------------

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
so generates the quotient. The sequence of two ``div`` ’s then plays
the role of a generator of all common divisors of ``a,b`` together with
the corresponding pairs of numbers which are ``a,b`` divided by their
common divisors. The ```gcd`` sub-relation `<peano.ml#L93>`__ finally
checks for the greatest common divisor, and the corresponding pair of
quotients is the answer for ``a',b'``.

In summary, generate-and-test is both a technique that the programmer
applies to solve certain problems (e.g., the ``gcd'`` case), and a usual
way in which relational programs search for answers even if the
programmer does not intentionally apply it (e.g., the ``simplify``
case).

The Formula Parser
------------------

In the library implementation and the test file, we often see formulae
enclosed by the ``ocanren{}`` quotation which takes care of, among
others, precedence and associativity of the logic connectives. It is an
OCanren-specific syntax extension which is described
:ref:`let-ocanren`


Building a Library
------------------

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
