[![Build Status](https://travis-ci.org/Jetbrains-Research/ocanren.svg?branch=master)](https://travis-ci.org/Jetbrains-Research/ocanren)

# Table of Contents

- [Introduction](#introduction)
  - [What is miniKanren](#what-is-minikenren)
  - [OCanren vs. miniKanren](#ocanren-vs-minikanren)
  - [Injecting and Projecting User-Type Data](#injecting-and-projecting-user-type-data)
  - [Bool, Nat, List](#bool-nat-list)
  - [Syntax Extensions](#syntax-extensions)
  - [Run](#run)
  - [Sample](#sample)
- [Building from sources](#building-from-sources)
  - [Building the library](#building-the-library)
  - [Building the syntax extensions](#building-the-syntax-extensions)
  - [Building and running tests](#building-and-running-tests)
  - [Building and running samples](#building-and-running-samples)
- [More info](#more-info)

# Introduction

`OCanren` is a strongly-typed embedding of relational programming language [miniKanren](http://minikanren.org) 
into [OCaml](http://ocaml.org). Nowadays, the implementation of `OCanren` strongly reminds [faster-miniKanren](https://github.com/michaelballantyne/faster-miniKanren).
Previous implementation was based on [microKanren](http://webyrd.net/scheme-2013/papers/HemannMuKanren2013.pdf)
with [disequality constraints](http://scheme2011.ucombinator.org/papers/Alvis2011.pdf).

<<<<<<< HEAD
## What is miniKanren

`miniKanren` is an embedded language for constraint/logic/relational programming.

<table>
<tr>
  <td>Prolog</td>
  <td>miniKanren</td>
  <td>OCanren</td>
</tr>

<tr valign="top">
  <td>

  ```prolog
  app([], X, X).       
  app([Y|Z], X, [Y,T]) :-
     app (Z, X, T).          
  ```
  
  </td>
  <td>

  ```scheme
  (define appendo              
   (lambda (l s ls)          
     (conde                    
      [(== '() l) (== s ls)]   
       [(fresh (a d res)      
         (== `(,a . ,d) l)     
         (== `(,a . ,res) ls) 
         (appendo d s res))])))
  ```

  </td>
  <td>

  ```ocaml
  let rec appendo x y z =
  ocanren {             
    x == [] & y == z |
    fresh h, t, ty in
      x == h :: t  &
      z == h :: ty &
      appendo t y ty
  }
  ```

  </td>
</tr>

<\table>

## OCanren vs. miniKanren

The correspondence between original miniKanren and OCanren constructs is shown below:

| miniKanren                        | OCanren                                         |
| --------------------------------- | ----------------------------------------------- |
| `#u`                              | success                                         |
| `#f`                              | failure                                         |
| `((==) a b)`                      | `(a === b)`                                     |
| `((=/=) a b)`                     | `(a =/= b)`                                     |
| `(conde (a b ...) (c d ...) ...)` | `conde [a &&& b &&& ...; c &&& d &&& ...; ...]` |
| `(fresh (x y ...) a b ... )`      | `fresh (x y ...) a b ...`                       |

In addition, OCanren introduces explicit disjunction (`|||`) and conjunction
(`&&&`) operators for goals.

## Injecting and Projecting User-Type Data

To make it possible to work with OCanren, user-type data have to be _injected_ into
logic domain. In the simplest case (non-parametric, non-recursive) the function

```ocaml
val inj  : ('a, 'b) injected -> ('a, 'b logic) injected
val lift : 'a ->  ('a, 'a) injected
```

can be used for this purpose:

```ocaml
inj @@ lift 1
```

```ocaml
inj @@ lift true
```

```ocaml
inj @@ lift "abc"
```

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

If the type is a (possibly recursive) algebraic type definition, then, as a rule, it has to be
abstracted from itself, and then we can write smart constructor for constructing
injected values,

```ocaml
type tree = Leaf | Node of tree * tree
```

is converted into

```ocaml
module T = struct
  type 'self tree = Leaf | Node of 'self * 'self

  let fmap f = function
  | Leaf -> Leaf
  | Node (l, r) -> Node (f l, f r)
end
include T
module F =  Fmap2(T)
include F

let leaf    ()  = inj @@ distrib @@ T.Leaf
let node   b c  = inj @@ distrib @@ T.Node (b,c)
```

Using fully abstract type we can construct type of `ground`
(without logic values) trees and type of `logic trees` --
the trees that can contain logic variables inside.

Using this fully abstract type and a few OCanren builtins we can
construct `reification` procedure which translates `('a, 'b) injected`
into it's right counterpart.

```ocaml
type gtree = gtree T.t
type ltree = ltree X.t logic
type ftree = (rtree, ltree) injected
```

Using another function `reify` provided by the functor application we can
translate `(_, 'b) injected` values to `'b` type.

```ocaml
val reify_tree : ftree -> ltree
let rec reify_tree eta = F.reify LNat.reify reify_tree eta
```

And using this function we can run query and get lazy stream of reified logic
answers

```ocaml
let _: Tree.ltree RStream.t =
  run q (fun q  -> q === leaf ())
        (fun qs -> qs#reify Tree.reify_tree)

```

<!--
Pragmatically speaking, it is desirable to make a type fully abstract, thus
logic variables can be placed in arbitrary position, for example,

```ocaml
type ('a, 'b, 'self) tree = Leaf of 'a | Node of 'b * 'self * 'self

let rec inj_tree t = !! (gmap(tree) (!!) (!!) inj_tree t)

```

instead of

```ocaml
type tree = Leaf of int | Node of string * t * t
```



Symmetrically, there is a projection function `prj` (and a prefix
synonym `!?`), which can be used to project logical values into
regular ones. Note, that this function is partial, and can
raise `Not_a_value` exception. There is failure-continuation-passing
version of `prj`, which can be used to react on this situation. See
autogenerated documentation for details.
-->

## Bool, Nat, List

There is some built-in support for a few basic types --- booleans, natural
numbers in Peano form, logical lists. See corresponding modules.

The following table summarizes the correspondence between some expressions
on regular lists and their OCanren counterparts:

| Regular lists | OCanren              |
| ------------- | -------------------- |
| `[]`          | `nil`                |
| `[x]`         | `!< x`               |
| `[x; y]`      | `x %< y`             |
| `[x; y; z]`   | `x % (y %< z)`       |
| `x::y::z::tl` | `x % (y % (z % tl))` |
| `x::xs`       | `x % xs`             |

## Syntax Extensions

There are two constructs, implemented as syntax extensions: `fresh` and `defer`. The latter
is used to eta-expand enclosed goal ("inverse-eta delay").

However, neither of them actually needed. Instead of `defer (g)` manual expansion can
be used:

```ocaml
(fun st -> Lazy.from_fun (fun () -> g st))
```

To get rid of `fresh` one can use `Fresh` module, which introduces variadic function
support by means of a few predefined numerals and a successor function. For
example, instead of

```ocaml
fresh (x y z) g
```

one can write

```ocaml
Fresh.three (fun x y z -> g)
```

or even

```ocaml
(Fresh.succ Fresh.two) (fun x y z -> g)
```

## Run

The top-level primitive in OCanren is `run`, which can be used in the following
pattern:

```ocaml
run n (fun q1 q2 ... qn -> g) (fun a1 a2 ... an -> h)
```

Here `n` stands for _numeral_ (some value, describing the number of arguments,
`q1`, `q2`, ..., `qn` --- free logic variables, `a1`, `a2`, ..., `an` --- streams
of answers for `q1`, `q2`, ..., `qn` respectively, `g` --- some goal, `h` --- a
_handler_ (some piece of code, presumable making use of `a1`, `a2`, ..., `an`).

There are a few predefined numerals (`q`, `qr`, `qrs`, `qrst` etc.) and a
successor function, `succ`, which can be used to "manufacture" greater
numerals from smaller ones.

## Sample

We consider here a complete example of OCanren specification (relational
binary search tree):

```ocaml
open Printf
open GT
open OCanren
open OCanren.Std

module Tree = struct
  module X = struct
    (* Abstracted type for the tree *)
    @type ('a, 'self) t = Leaf | Node of 'a * 'self * 'self with gmap,show;;
    let fmap eta = GT.gmap t eta
  end
  include X
  include Fmap2(X)

  @type inttree = (int, inttree) X.t with show
  (* A shortcut for "ground" tree we're going to work with in "functional" code *)
  @type rtree = (LNat.ground, rtree) X.t with show

  (* Logic counterpart *)
  @type ltree = (LNat.logic, ltree) X.t logic with show

  type ftree = (rtree, ltree) injected

  let leaf    () : ftree = inj @@ distrib @@ X.Leaf
  let node a b c : ftree = inj @@ distrib @@ X.Node (a,b,c)

  (* Injection *)
  let rec inj_tree : inttree -> ftree = fun tree ->
     inj @@ distrib @@ GT.(gmap t nat inj_tree tree)

  (* Projection *)
  let rec prj_tree : rtree -> inttree =
    fun x -> GT.(gmap t) LNat.to_int prj_tree x

end

open Tree

(* Relational insert into a search tree *)
let rec inserto a t' t'' = conde [
  (t' === leaf ()) &&& (t'' === node a (leaf ()) (leaf ()) );
  fresh (x l r l')
    (t' === node x l r)
    Nat.(conde [
      (t'' === t') &&& (a === x);
      (t'' === (node x l' r  )) &&& (a < x) &&& (inserto a l l');
      (t'' === (node x l  l' )) &&& (a > x) &&& (inserto a r l')
    ])
]

(* Top-level wrapper for insertion --- takes and returns non-logic data *)
let insert : int -> inttree -> inttree = fun a t ->
  prj_tree @@ RStream.hd @@
  run q (fun q  -> inserto (nat a) (inj_tree t) q)
        (fun qs -> qs#prj)

(* Top-level wrapper for "inverse" insertion --- returns an integer, which
   has to be inserted to convert t into t' *)
let insert' t t' =
  LNat.to_int @@ RStream.hd @@
  run q (fun q  -> inserto q (inj_tree t) (inj_tree t'))
        (fun qs -> qs#prj)

(* Entry point *)
let _ =
  let insert_list l =
    let rec inner t = function
    | []    -> t
    | x::xs ->
      let t' = insert x t in
      printf "Inserting %d into %s makes %s\n%!" x (show_inttree t) (show_inttree t');
      inner t' xs
    in
    inner Leaf l
  in
  ignore @@ insert_list [1; 2; 3; 4];
  let t  = insert_list [3; 2; 4; 1] in
  let t' = insert 8 t in
  Printf.printf "Inverse insert: %d\n" @@ insert' t t'
```

## Camlp5 syntax extensions

A few syntax extensions are used in this project.

For testing we use the one from `logger-p5` opam package. It allows to convert OCaml
expression to its string form. For example, it rewrites `let _ = REPR(1+2)` to

```
$ camlp5o `ocamlfind query logger`/pa_log.cmo pr_o.cmo a.ml
let _ = "1 + 2", 1 + 2
```

For OCanren itself we use syntax extension to simplify writing relational programs

```
$  cat a.ml
let _ = fresh (x) z
$  camlp5o _build/camlp5/pa_ocanren.cmo pr_o.cmo a.ml
let _ = OCanren.Fresh.one (fun x -> delay (fun () -> z))
```

## PPX syntax extensions

PPX syntax extensions are not related to camlp5 and should be used, for example,
if you want decent IDE support. Main extensions are compilable by `make ppx`

An analogue for logger library is called `ppx_repr`:

```
$ cat regression_ppx/test002repr.ml
let _ = REPR(1+2)
$ ./pp_repr.native regression_ppx/test002repr.ml
let _ = ("1 + 2", (1 + 2))
$ ./pp_repr.native -print-transformations
repr
```

An OCanren-specific syntax extension includes both `ppx_repr` and extension for
creating fresh variables

```
$ cat a.ml
let _ = fresh (x) z
$ ./pp_ocanren_all.native a.ml
let _ = OCanren.Fresh.one (fun x -> delay (fun () -> z))
$ ./pp_ocanren_all.native -print-transformations
pa_minikanren
repr
```

There also syntax extensions for simplifyng developing data type for OCanren
but they are not fully documented.

# Building from sources

This section contains instructions on how to build OCanren from sources and how to build and run tests.

## Building the library

OCanren uses [dune](https://dune.readthedocs.io/en/stable/) build system.
In order to build the library only, type `dune build src`
(alternatively, use `make lib`, which is a shortcut for the previous command).

You can build only the bytecode version of the library by `dune build src/OCanren.cma`
or only the native version by `dune build src/OCanren.cmxa`.

## Building the syntax extensions

The `ppx` syntax extensions can be built by the command `dune build ppx ppxnew` (or `make ppx`).

The `camlp5` syntax extensions is built by the command `dune build camlp5` (or `make syntax`),

## Building and running tests

To build all tests type `dune build regression`.

In order to build the single test use `dune build regression/testname.exe`.

Tests can be run via the custom script `test.sh`.
This script builds the given test, runs it, saves its output
to the file `regression/testname.log` and compares it
to the expected output from the `regression/orig/testname.orig` file.
In case of the mismatch it will save the diff of two files into `regression/testname.diff`

To run all tests type (while in the root folder of the project)
`./test.sh all` (or, alternatively, `make test`).
In order to run the single test type `./test.sh testname`
where `testname` is the name of one of the tests from the `regression` folder.
E.g. `./test.sh test000` will build and run `regression/test000.exe`
producing files `regression/test000.log` and `regression/test000.diff`.

It is possible to promote the output of the test and make it the new 'original'.
In order to do that type `./test.sh --promote testname`.
This command will copy the content of the `regression/testname.log` file into `regression/orig/testname.orig`.
In order to promote all tests, use `./test.sh --promote all` (or `make promote`).

To remove all `*.log` and `*.diff` files use `make clean-test`.

## Building and running samples

To build samples type `dune build samples` (or `make samples`).
To build the single sample type `dune build samples/prog.exe` 
where `prog` is the name of the sample.
Samples can be run via `dune exec samples/prog.exe` 
(e.g. `dune exec samples/tree.exe`).

Also, the output of the sample can be compared to the expected one (and promoted) by the script `./test.sh`
(e.g. `./test.sh tree` will run the sample `tree.exe` and compare its output against `samples/orig/tree.orig`).

# More info

See autogenerated documentation or samples in `/regression` and `/samples` subdirectories.
