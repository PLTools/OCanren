Camlp5 syntax extensions
========================

A few syntax extensions are used in this project.

For testing we use the one from ``logger-p5`` opam package. It allows to convert OCaml
expression to its string form. For example, it rewrites ``let _ = REPR(1+2)`` to

.. code-block::

   $ camlp5o `ocamlfind query logger`/pa_log.cmo pr_o.cmo a.ml
   let _ = "1 + 2", 1 + 2

For OCanren itself we use syntax extension to simplify writing relational programs

.. code-block::

   $  cat a.ml
   let _ = fresh (x) z
   $  camlp5o _build/camlp5/pa_ocanren.cmo pr_o.cmo a.ml
   let _ = OCanren.Fresh.one (fun x -> delay (fun () -> z))

.. _ocanren-vs-miniKanren:

OCanren vs. miniKanren
----------------------

The correspondence between original miniKanren and OCanren constructs is shown below:

.. list-table::
   :header-rows: 1

   * - miniKanren
     - OCanren
   * - ``#u``
     - success
   * - ``#f``
     - failure
   * - ``((==) a b)``
     - ``(a === b)``
   * - ``((=/=) a b)``
     - ``(a =/= b)``
   * - ``(conde (a b ...) (c d ...) ...)``
     - ``conde [a &&& b &&& ...; c &&& d &&& ...; ...]``
   * - ``(fresh (x y ...) a b ... )``
     - ``fresh (x y ...) a b ...``



In addition, OCanren introduces explicit disjunction (``|||``) and conjunction
(``&&&``) operators for goals.

.. _let-ocanren:

OCanren-specific Camlp5 syntax extension
----------------------------------------

(Also known as regular extension)

This section is a part of Yue Li's tutorial on OCanren.

We take a
look at the
`implementation <https://github.com/JetBrains-Research/OCanren/blob/master/camlp5/pa_ocanren.ml>`__ of
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

This code links with Camlp5 libraries, in particular the one that provide ways to extend syntax: `pa_extend`.
Loading them amounts to extending the OCaml syntactic category
`expression <https://ocaml.org/releases/4.11/htmlman/expr.html>`__ with
several sub-categories one of which is named *extend*:

.. code::

   expr = ... | extend ;
   extend = "EXTEND", extend-body, "END" ;

An expression that belongs to the category “extend” would be called an
*EXTEND statement*.

Our formula parser has only
`one <https://github.com/JetBrains-Research/OCanren/blob/master/camlp5/pa_ocanren.ml#L167>`__ EXTEND
statement, whose extend-body starts with a `global
indicator <https://github.com/JetBrains-Research/OCanren/blob/master/camlp5/pa_ocanren.ml#L168>`__
followed by a semicolon separated list of *entries* (whose names are,
exhaustively,
`long_ident <https://github.com/JetBrains-Research/OCanren/blob/master/camlp5/pa_ocanren.ml#L170>`__,
`expr <https://github.com/JetBrains-Research/OCanren/blob/master/camlp5/pa_ocanren.ml#L185>`__,
`ocanren_embedding <https://github.com/JetBrains-Research/OCanren/blob/master/camlp5/pa_ocanren.ml#L221>`__,
`ocanren_expr <https://github.com/JetBrains-Research/OCanren/blob/master/camlp5/pa_ocanren.ml#L225>`__,
`ocanren_term <https://github.com/JetBrains-Research/OCanren/blob/master/camlp5/pa_ocanren.ml#L254>`__,
`ocanren_term' <https://github.com/JetBrains-Research/OCanren/blob/master/camlp5/pa_ocanren.ml#L258>`__
and
`ctyp <https://github.com/JetBrains-Research/OCanren/blob/master/camlp5/pa_ocanren.ml#L289>`__). An
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
`decapitalize <https://github.com/JetBrains-Research/OCanren/blob/master/camlp5/pa_ocanren.ml#L170>`__,
`ctor <https://github.com/JetBrains-Research/OCanren/blob/master/camlp5/pa_ocanren.ml#L43>`__ and
`fix_term <https://github.com/JetBrains-Research/OCanren/blob/master/camlp5/pa_ocanren.ml#L56>`__
etc.

How it works
^^^^^^^^^^^^

The syntax extension kit ``pa_extend`` is fundamental for the
cascade of extensions described below. The entries ``expr`` and ``ctyp``
origin from the module Pcaml that is the core of Camlp5 and is
`opened <../../Installation/ocanren/camlp5/pa_ocanren.ml#L37>`__ by the
formula parser. Pcaml initializes the (empty) grammar entries ``expr``
and ``ctyp``. The standard OCaml parsing kit of Camlp5 then defines them
by means of an EXTEND statement and accordng to the standard syntax of
OCaml. Our EXTEND statement further extends these global entries with
locally defined entries — entries other than ``expr`` and ``ctyp`` in
our EXTEND statement are locally defined, such as ``ocanren_embedding``,
``ocanren_expr`` and ``ocanren_term`` etc. Below you can find
three stages of extension with links to documentation
..  The following table summarizes the stages of extension, providing links to copies of relevant files from either OCanren  ource or Camlp5 source, together with their documentations.

1. Initialization is documented in `The Pcaml module <https://camlp5.github.io/doc/htmlc/pcaml.html>`__ and happend in file `pcaml.ml` : `expr <https://github.com/camlp5/camlp5/blob/rel8.00.02/main/pcaml.ml#L57>`__ and `ctyp <https://github.com/camlp5/camlp5/blob/rel8.00.02/main/pcaml.ml#L60>`__.
2. Parsing Kit for Standard OCaml `pa_o.ml` : `expr <https://github.com/camlp5/camlp5/blob/rel8.00.02/etc/pa_o.ml#L1193>`__, `ctyp  <https://github.com/camlp5/camlp5/blob/rel8.00.02/etc/pa_o.ml#L1971>`__
3. OCanren Formula Parser `pa_ocanren.ml`: `expr <https://github.com/JetBrains-Research/OCanren/blob/master/camlp5/pa_ocanren.ml#L185>`__, `ctyp <https://github.com/JetBrains-Research/OCanren/blob/master/camlp5/pa_ocanren.ml#L289>`__


As a preprocessing tool, Camlp5 defines its own parser ``pa_o.ml`` for
standard OCaml, so that any standard OCaml code can be converted by it
into an AST recongnizable by the `OCaml
compiler <https://ocaml.org/releases/4.11/htmlman/comp.html>`__. Is
``pa_o.ml`` a redundant piece of work for we can just use the OCaml
compiler to build the AST? Not exactly, because besides ``pa_o.ml``,
Camlp5 also provides EXTEND statments so that syntactic categories
defined in ``pa_o.ml`` can be extended. The result is that using the
combination of ``pa_ocanren.ml`` and ``pa_o.ml`` we can convert code
that is not wholly in OCaml into a purely OCaml AST.

Conclusion
^^^^^^^^^^

The OCanren formula parser has the EXTEND statement as its core, which
consists of a list of entries, notably the global entries ``expr`` and
``ctyp`` that extend the corresponding predefined entries that conform
to standard OCaml. Such extension is characterized by the locally
defined entries such as ``ocanren_embedding``.

We will next focus on the extension of ``expr`` and leave ``ctyp``.
As far as the semantics is concerned entries are
parsers for syntactic categories. From now on we use the words “entry”
and “parser” interchangeably.

The global entry: ``expr``
~~~~~~~~~~~~~~~~~~~~~~~~~~

This is the major entry of the OCanren formula parser, which starts like:

.. code:: ocaml

   expr: LEVEL "expr1" [ ...

where we see the entry name *expr* and the position ``LEVEL "expr1"``.
We now use OCanren-expr to refer to the ``expr`` entry in the
OCanren formula parser, and OCaml-expr to refer to the predefined
entry ``expr`` in the Camlp5 parsing kit for standard OCaml.
OCanren-expr extends OCaml-expr in the position
``LEVEL "expr1"`` : the first level of the OCanren-expr is merged
with the `level named “expr1” <camlp5_src_ref/pa_o.ml#L563>`__ of the
OCaml-expr, i.e., their rules are put together and grouped as a
single level named “expr1”; other levels from OCanren-expr are
inserted into OCaml-expr as new levels, right below the extended
“expr1” level. There are three levels in the OCanren-expr, the third
of which is:

.. code:: ocaml

   [ e=ocanren_embedding -> e ]

This third level of OCanren-expr is inserted as a new level in
OCaml-expr, and the entry ``ocanren_embedding`` directly corresponds
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
braces).

1. The `first level <https://github.com/JetBrains-Research/OCanren/blob/master/camlp5/pa_ocanren.ml#L226>`__ parses a
disjunction:

.. code:: ocaml

    "top" RIGHTA [ l=SELF; "|"; r=SELF -> <:expr< OCanren.disj $l$ $r$ >> ]

2. The `second level <https://github.com/JetBrains-Research/OCanren/blob/master/camlp5/pa_ocanren.ml#L227>`__ parses a conjunction:

.. code:: ocaml

    RIGHTA [ l=SELF; "&"; r=SELF -> <:expr< OCanren.conj $l$ $r$ >> ]

3. The `third level <https://github.com/JetBrains-Research/OCanren/blob/master/camlp5/pa_ocanren.ml#L2289>`__ parses a
fresh variable introduction (i.e., existential quantification):

.. code:: ocaml

    [ "fresh"; vars=LIST1 LIDENT SEP ","; "in"; b=ocanren_expr LEVEL "top" ->
       List.fold_right
         (fun x b ->
            let p = <:patt< $lid:x$ >> in
            <:expr< OCanren.call_fresh ( fun $p$ -> $b$ ) >>
         )
         vars
         b
    ]

4. The `fourth level <../../Installation/ocanren/camlp5/pa_ocanren.ml#L238>`__ parses
atomic, named and grouped formulae (and else):

.. code:: ocaml

    "primary" [
        p=prefix; t=ocanren_term                      -> let p = <:expr< $lid:p$ >> in <:expr< $p$ $t$ >>
      | l=ocanren_term; "==" ; r=ocanren_term         -> <:expr< OCanren.unify $l$ $r$ >>
      | l=ocanren_term; "=/="; r=ocanren_term         -> <:expr< OCanren.diseq $l$ $r$ >>
      | l=ocanren_term; op=operator; r=ocanren_term   -> let p = <:expr< $lid:op$ >> in
                                                         let a = <:expr< $p$ $l$ >> in
                                                         <:expr< $a$ $r$ >>
      | x=ocanren_term                                -> x
      | "{"; e=ocanren_expr; "}"                      -> e
      (* other rules omitted *)
      ...
    ]

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
``s (s (o ()))`` :

.. code:: ocaml

   ocanren { fresh x in S (S O) == x }

Such conversion bridges the gap between the programmer’s intuition of
writing OCaml values and OCanren’s internal representation of the same
values, Inspecting the entries ``ocanren_term``, ``ocanren_term'`` and
their auxiliary functions help us know precisely how the conversion is
performed.

Below is the definition of the entry ``ocanren_term`` :

.. code:: ocaml

   ocanren_term: [[ t=ocanren_term' -> fix_term t ]];

where the ``ocanren_term'`` parser is called immediately to process
expressions like ``S (S O)`` and the intermediate result (an AST) is
bound to the pattern variable ``t`` and then passed to the auxiliary
function ``fix_term``. The AST returned by ``fix_term`` is returned by
the parser ``ocanren_term``.

The ``ocanren_term'`` parser has four levels, namely:

1. `“app” <https://github.com/JetBrains-Research/OCanren/blob/master/camlp5/pa_ocanren.ml#L259>`__, for
applications.

.. code:: ocaml

    "app"  LEFTA  [ l=SELF; r=SELF -> <:expr< $l$ $r$ >> ]``

Applications are treated as being left associative as indicated by
``LEFTA``. This level not yet converts constructor applications into
injection function applications. Instead it simply builds the AST of the
application in a straightforward manner, not distinguishing a
constructor application from a function application.

2. `“list” <../../Installation/ocanren/camlp5/pa_ocanren.ml#L261>`__ , for non-empty lists with ``::`` as the top level constructor.

.. code:: ocaml

    "list" RIGHTA [ l=SELF; "::"; r=SELF -> <:expr< OCanren.Std.List.cons $l$ $r$ >> ]

The constructor ``::`` is replaced by the OCanren standard library
function `cons <../../Installation/ocanren/src/std/LList.mli#L47>`__
which is the injection function for the constructor
`OCanren.Std.List.Cons <../../Installation/ocanren/src/std/LList.mli#L27>`__.

3. `“primary” <../../Installation/ocanren/camlp5/pa_ocanren.ml#L262>`__,
which has rules for:

    - `anti-quotations <../../Installation/ocanren/camlp5/pa_ocanren.ml#L262>`__

    .. code:: ocaml

        "!"; "("; e=expr; ")" -> e

    So that the ``ocanren{}`` quotation would take any ``<value>`` from ``!(<value>)`` as is without further processing. In other words, the ``<value>`` will be parsed using the entry ``expr``.

    - `integers <../../Installation/ocanren/camlp5/pa_ocanren.ml#L263>`__

    .. code:: ocaml

        c=INT -> let n = <:expr< $int:c$ >> in <:expr< OCanren.Std.nat $n$ >>

    Thus, occurrences of integers like ``15`` within the ``ocanren{}`` quotation would be converted to values of the Peano number type that is provided by the OCanren standard library `OCanren.Std.Nat <../../Installation/ocanren/src/std/LNat.mli>`__.

    - `characters <../../Installation/ocanren/camlp5/pa_ocanren.ml#L266>`__ and `strings <../../Installation/ocanren/camlp5/pa_ocanren.ml#L269>`__

    .. code:: ocaml

        c=CHAR   -> let s = <:expr< $chr:c$ >> in <:expr< OCanren.inj (OCanren.lift $s$) >>      | s=STRING -> let s = <:expr< $str:s$ >> in <:expr< OCanren.inj (OCanren.lift $s$) >>

    Characters and strings are injected using the primary injection function ``!!`` (see its `signature <../../Installation/ocanren/src/core/Logic.mli#L57>`__ and `implementation <../../Installation/ocanren/src/core/Logic.ml#L65>`__).

    - `booleans <../../Installation/ocanren/camlp5/pa_ocanren.ml#L272>`__

    .. code:: ocaml

        | true"   -> <:expr< OCanren.Std.Bool.truo >>
        | "false"  -> <:expr< OCanren.Std.Bool.falso >>

    Boolean values are converted into the corresponding injected values from the OCanren standard library `LBool <../../Installation/ocanren/src/std/LBool.mli#L45>`__.



    - lists delimited by ``[]`` and ``;`` <https://github.com/JetBrains-Research/OCanren/blob/master/camlp5/pa_ocanren.ml#L273>`__

    .. code:: ocaml

        "["; ts=LIST0 ocanren_term' SEP ";"; "]" ->
          ( match ts with
          | [] -> <:expr< OCanren.Std.nil () >>
          | _  ->
              List.fold_right (fun x l -> <:expr< OCanren.Std.List.cons $x$ $l$ >> )
              ts
              <:expr< OCanren.Std.nil () >>
          )

    The entry `ocanren_term'` is recursively called to process the list members and the injection functions for list constructors are applied.

    - `operators <https://github.com/JetBrains-Research/OCanren/blob/master/camlp5/pa_ocanren.ml#L278>`__ (which are not qualified)

    .. code:: ocaml

        "("; op=operator_rparen -> <:expr< $lid:op$ >>

    Operators are specified by the auxiliary function ```is_operator`` <.https://github.com/JetBrains-Research/OCanren/blob/master/camlp5/pa_ocanren.ml#L87>`__ and extracted by another auxiliary function `operator_rparen <https://github.com/JetBrains-Research/OCanren/blob/master/camlp5/pa_ocanren.ml#L99>`__ (the name of which reads “operator right parenthesis”).

    - (possibly empty) `tuples <https://github.com/JetBrains-Research/OCanren/blob/master/camlp5/pa_ocanren.ml#L279>`__

    .. code:: ocaml

        "("; ts=LIST0 ocanren_term' SEP ","; ")" ->
        (match ts with
          | []  -> <:expr< OCanren.inj (OCanren.lift ()) >>
          | [t] -> t
          | _   -> <:expr< ( $list:ts$ ) >> )

    There is a recursive call of the entry itself to process members of the tuple, and then the AST of the tuple is built.

1. The level for long identifiers.

    .. code:: ocaml

        [ long_ident ]

This level calls the entry `long_ident <../../Installation/ocanren/camlp5/pa_ocanren.ml#L171>`__ to build AST’s of (possibly qualified) upper / lower case identifiers and operators which are taken as is.


Therefore, given ``S (S O)`` the ``ocanren_term'`` parser would return a straightforward translation into an AST. The interesting thing is done by ``fix_term`` and its helper ``ctor`` (read “C-tour”). The latter tests the input: if it is a (possibly qualified) uppercase identifier then sets the initial letter to lowercase and wraps the whole thing by
``Some``, e.g., ``Mod1.Mod2.ABC`` becomes (roughly) ``Some Mod1.Mod2.aBC`` ; if the input is not a (qualified) uppercase
identifier then returns ``None`` :

.. code:: ocaml

   let rec ctor e =
     let loc = MLast.loc_of_expr e in
     match e with
     | <:expr< $uid:u$ >>   -> Some (<:expr< $lid:decapitalize u$ >>)
     | <:expr< $m$ . $e$ >> -> (match ctor e with Some e -> Some (<:expr< $m$ . $e$ >>) | _ -> None)
     | _                    -> None

The `fix_term <https://github.com/JetBrains-Research/OCanren/blob/master/camlp5/pa_ocanren.ml#L56>`__ function then recurses down the structure of applications to systematically replace uppercase identifiers with lowercase identifiers
produced by ``ctor``. After a constant constructor is changed to lowercase, it is provided with the unit value ``()`` as the argument, e.g., ``O`` becomes ``o ()``. A non-constant constructor is not only set to lowercase, but also has its argument list transformed, e.g., ``Cons(a,b)`` becomes (roughly) ``cons a b``. Tuples are also replaced
by their OCanren standard library counterpart — `logic tuples <../../Installation/ocanren/src/std/LPair.mli>`__.

These lowercase identifiers converted from constructors are supposed to
be injection functions, which must be defined by the programmer
somewhere in the program, otherwise there would be some compile-time
error like “unbound identifier”. This explains why the injection
function names are always differ from the corresponding constructor
names by one letter: the initial letter.
