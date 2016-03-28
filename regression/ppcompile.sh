#!/bin/bash

FILE=$1
FILENAME=`basename $FILE .ml`
PP_POSTFIX="_pp.ml"
FILE_PP=$FILENAME$PP_POSTFIX

camlp5o pr_o.cmo pa_log.cmo pa_minikanren.cmo pa_gt.cmo -L `camlp5 -where` $FILE -o $FILE_PP

ocamlc -rectypes -I `ocamlfind -query typeutil` -I `ocamlfind -query GT` -I `ocamlfind -query MiniKanren` typeutil.cma GT.cma MiniKanren.cma -o `basename $FILE .ml` tester.ml $FILE_PP
