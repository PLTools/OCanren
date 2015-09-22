ocamlc -c -pp "camlp5o " -I `ocamlfind -query GT` -I `camlp5o -where` GT.cma minikanren.ml
ocamlc -rectypes -c Stream.ml
ocamlc -rectypes -c -pp "camlp5o pa_gt.cmo -L `camlp5o -where` -L ." -I `ocamlfind -query typeutil` -I `ocamlfind -query GT` MiniKanren.ml
ocamlc -rectypes -o test -pp "camlp5o pa_gt.cmo -L `camlp5o -where` -L ." -I `ocamlfind -query typeutil` -I `ocamlfind -query GT` typeutil.cma GT.cma Stream.cmo MiniKanren.cmo test.ml

