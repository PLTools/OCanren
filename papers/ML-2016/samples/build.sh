ocamlopt -rectypes -o sort -pp "camlp5o pa_minikanren.cmo pa_log.cmo pa_gt.cmo -L `camlp5 -where`" -I `ocamlfind -query GT` -I `ocamlfind -query MiniKanren` GT.cmxa MiniKanren.cmxa sort.ml
