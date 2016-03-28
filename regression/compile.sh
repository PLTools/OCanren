#!/bin/bash

FILE=$1

ocamlc -rectypes -pp "camlp5o pr_o.cmo pa_log.cmo pa_minikanren.cmo pa_gt.cmo -L `camlp5 -where`" -I `ocamlfind -query typeutil` -I `ocamlfind -query GT` -I `ocamlfind -query MiniKanren` typeutil.cma GT.cma MiniKanren.cma -o `basename $FILE .ml` tester.ml $FILE
