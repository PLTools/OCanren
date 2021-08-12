Say “Hello World!” in OCanren
=============================

We first execute the program, and then we read and explain through the
source code line by line.


Executing the Program
~~~~~~~~~~~~~~~~~~~~~

The `source code <https://github.com/YueLiPicasso/OCanrenTutorial/tree/main/Main>`_ has to be compiled and linked, for which you would need
the `Makefile <Makefile>`__. Now open terminal under your local copy ofthe ``helloWorld`` directory, and:

::

   make

This would produce a native-code file ``hello.opt``, execution of which by:

::

   ./hello.opt

will print ``hello world!`` in your terminal.

Reading the Program
~~~~~~~~~~~~~~~~~~~

The first line:

.. code:: ocaml

   open OCanren

makes the names from the module OCanren available for later use.

Important Interfaces
~~~~~~~~~~~~~~~~~~~~

The source code of the module OCanren resides in
`OCanren.ml <https://github.com/JetBrains-Research/OCanren/tree/0.2.0/src>`__ (It does not
have an accompanying ``.mli`` file). Inspecting the content thereof, we
shall see that basically it includes three modules
`Logic <https://github.com/JetBrains-Research/OCanren/tree/0.2.0/src/core/Logic.mli>`__,
`Core <https://github.com/JetBrains-Research/OCanren/tree/0.2.0/src/core/Core.mli>`__ and
module `Stream <https://github.com/JetBrains-Research/OCanren/tree/0.2.0/src/core/RStream.mli>`__, and finally defines the Std module. You should open the module
OCanren at the beginning of every source file where you use OCanren
features.

The second line:

.. code:: ocaml

   let str = !!("hello world!\n")

associates the value name ``str`` with the expression that is the prefix
operator ``!!`` (named *primitive injection*) applied to the string
literal ``"hello world!\n"``.

Internal Representation
~~~~~~~~~~~~~~~~~~~~~~~
The operator ``!!``, provided by the module Logic, makes type conversion
to the OCanren internal representation, something like what a calculator
program does when it receives an input string “1” and converts it to the
integer or floating-point type for further processing. We could see from
the interface that:

.. code:: ocaml

   val (!!) : 'a -> ('a, 'a logic) injected

The ``injected`` type constructor is provided by the module Logic as an
abstract type (so we do not concern ourselves with its implementation).

More information about various typed being used and the meaning of type
parameters of the `('a, 'b) injected` could be get from
:ref:`Digesting the Types`.

Logic Variables
^^^^^^^^^^^^^^^

The ``logic`` type constructor which appears in the type of ``!!`` above
is also provided by the module ``OCanren.Logic``. It takes one type
parameter and its type representation is exposed: we could see from the
module interface that it has two constructors ``Var`` and ``Value``,
representing respectively a *logic variable* and a *concrete value*
over/of the parameter type, in the sense that wrt. the arithmetic
expression ``1 + x`` we say that ``x`` is a logic variable over the
integer type and ``1`` is a concrete value of the integer type.

Making a Query
~~~~~~~~~~~~~~

The 3rd line:

.. code:: ocaml

   let _ =
     List.iter print_string @@
     Stream.take ~n:1 @@
     run q (fun q -> ocanren { q == str }) project

is divided into three sub-expressions by the right associative infix
operator ``@@`` that is provided by OCaml’s core library
`Stdlib <http://caml.inria.fr/pub/docs/manual-ocaml/libref/Stdlib.html>`__.
The most important sub-expression is:
``run q (fun q -> ocanren { q == str }) project`` whose most
important part is: ``q == str``.

.. note::

   The piece of code on the discussion uses an OCanren-specific syntax extension that doesn't appear in other languages of miniKanren family. See :ref:`let-ocanren` for details.

Next we start with explaining the inner most and the most important
part: ``q == str``, followed by its immediate enclosing environment
which is the *run* statement:

.. code:: ocaml

   run q (fun q -> ocanren { q == str }) project

and finally the top most expression for taking and printing answers.

Syntactic Identity and Unification
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


Syntactic identity between two expressions :math:`expr_1` and :math:`expr_2` (of
the same type) is denoted using two equation symbols: :math:`expr_1 == expr_2`. Usually we are
given two different expressions both of which have zero or more
sub-expressions considered as logic variables, and we are interested in
finding (type sensitive) substitutes for these logic variables so that
the resulting expressions are syntactically identical. Finding such
substitutes is know as *unification* of the original expressions.

**Example** In order for both ``(x + 1) == (2 + y)`` and
``Node (x,1) == Node (2,y)`` to be true, we replace ``x`` by ``2`` and
``y`` by ``1``, making both sides of ``==`` the expression ``2 + 1`` or
``Node (2,1)`` respectively. We now unified ``(x + 1)`` with
``(2 + y)``. Moreover, ``Node (x,1)`` is unified with ``Node (2,y)``.

**Example** How to substitute the logic variable ``z`` so that
``z == "hello world!"`` ? Trivial: replacing ``z`` with the constant
``"hello world!"``. This is essentially what our program does: solving a
unification problem.

The OCanren Top Level: the *run* expression
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We can parse the ``run ...`` expression following the syntax below,
which is given in
`EBNF <https://github.com/YueLiPicasso/language-grammars>`__ except that
occurrences of the meta-identifier ``etc`` signifies omission: there is
no single syntactic category named ``etc``.

.. code:: ebnf

   top level = 'run',  size indicator, goal, answer handler ;

   size indicator =  'one' | 'two' | 'three' | 'four' | 'five'
                   | 'q'   | 'qr'  | 'qrs'   | 'qrst' | 'qrstu'
           | '(', size indicator, ')'
           | 'succ', size indicator ;

   goal = 'fun', parameters, '->', goal body ;

   parameters = etc ;

   goal body = 'ocanren', '{', pretty goal body, '}' ;

   pretty goal body = etc ;

   answer handler = 'project' | etc ;

A goal asks: what values shall be assumed by the parameters so that the
proposition as given by the goal body (in which these parameters are
expected to occur) holds?

The ``ocanren { }`` environment in a goal body instructs the
`Camlp5 <https://camlp5.github.io/>`__ preprocessor to transform on the
syntactic level the ``pretty goal body`` into the more verbose and less
intuitive calls of OCanren's functions. As a reference, the rules by
which the preprocessing is done is given in ``pa_ocanren.ml``
`where <../../Installation/ocanren/camlp5/pa_ocanren.ml#L238>`__ we
could find, for example, the ``==``  symbol is transformed into the name
``unify``.

.. todo::

   fix links

The number of parameters shall agree with the size indicator, where
``q`` … ``qrstu`` are just alternative names for ``one`` … ``five``
respectively. If there are more than five parameters, the successor
function ``succ`` can be applied to build larger size indicators, e.g.,
``(succ (succ five))`` is for seven parameters.

The answer handler is a type converter from the OCanren internal
representation to a user-level representation. When there is no free
logic variables in the answer we use ``project``. The ``Not_a_value``
exeption (provided by OCanren.Logic) is thrown if we use ``project`` as
the handler but the answer contains free logic variables: in this case
some other handler shall be used.


.. todo::

   Reference reifiers

The ``run`` function and the size indicators are provided by module ``OCanren.Core``. Basic
answer handlers are provided by module ``OCanren.Logic``.

Taking and Displaying Answers
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The top level constructs a lazy stream out of which an arbitrary number
of answers could be pulled, subject to answer availability. We use a
stream instead of a finite list to hold the answers because generally
the set of all answers is enumerable. Omission of the optional argument
``~n`` of ``take`` means “take all”. Finally we use OCaml standard
functions to iterate through the taken list of answers and print each
one in the terminal. Our program has only one answer.
