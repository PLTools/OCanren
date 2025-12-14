  $ ls
  $ echo 'ocanren type x = X' > x.ml
  $ ls `ocamlfind query camlp5`/*.cmo
  $ ocamlfind c -pp 'camlp5 -I `ocamlfind query camlp5` pa_o.cmo o_keywords.cmo pr_o.cmo ./pa_ocanren.cma' -dsource x.ml