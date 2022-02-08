(lang dune 2.8)

(cram enable)

(generate_opam_files true)

(license LGPL-2.1)

(source
 (github JetBrains-Research/OCanren))

(name OCanren)

(package
 (version 0.3.0)
 (name OCanren)
 (synopsis "Implementation of miniKanren relational (logic) EDSL")
 (description
"\| The family of miniKanren derivatives has many implementations. This
"\| one is typed embedding to OCaml. The original implementation for
"\| this is in scheme and can be found at https://github.com/michaelballantyne/faster-miniKanren .
"\|
"\| Features:
"\| 1) disequality constraints
"\| 2) relational standard library: lists, nats
)
 (depends
  (ocaml
   (>= 4.10))
   dune-configurator
   ocamlfind
   camlp5
   logger-p5
   (GT (>= 0.4))
   (benchmark :with-test)
   (ocamlformat (and :with-test (<= 0.20)))
   ppx_inline_test
   mtime)
 (authors
  "Dmitrii Boulytchev https://github.com/dboulytchev"
  "Dmitrii Kosarev https://github.com/Kakadu"
  "Evgenii Moiseenko https://github.com/eupp"
  "Petr Lozov https://github.com/Lozov-Petr"
  "JetBrains Research https://research.jetbrains.org")
 (bug_reports "https://github.com/JetBrains-Research/OCanren/issues")
 (maintainers "Dmitrii Kosarev Kakadu@pm.me"))

(package
 (version 0.3.0)
 (name OCanren-ppx)
 (synopsis
  "Implementation of miniKanren relational (logic) EDSL: PPX extensions")
 (description
"\| PPX rewriters for writing relational programs more conveniently.
"\|
"\| 1) `ppx_fresh` -- a clone of original miniKanren syntax
"\|
"\|   fresh (x...) goal
"\|
"\| expands into
"\|
"\|   Fresh.numeral (fun x ... -> goal)
"\|
"\| 2) `ppx_distrib` for easier generation of type-safe wrappers: distrib functor, etc...
"\|
"\|   [%%distrib
"\|     type t = fully_abstract_typee
"\|     type ground = ...
"\|   ]
"\|
"\| 3) `ppx_repr` for test. Expands `REPR(expr)` to `(string_of_expr expr, expr)`
)
 (depends
  (ocaml
   (>= 4.10))
  dune-configurator
  (ppxlib
   (and
    (>= 0.22)
    (<= 0.24)))
  base
  ppx_inline_test
  ppx_expect)
 (authors
  "Dmitrii Kosarev https://github.com/Kakadu"
  "JetBrains Research https://research.jetbrains.org")
 (bug_reports "https://github.com/JetBrains-Research/OCanren/issues")
 (maintainers "Dmitrii Kosarev Kakadu@pm.me"))

