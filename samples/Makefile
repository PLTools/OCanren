.PHONY: ppx_demo
PPX_DRIVER_RUNNER=ppx_runner

all: ppx_demo
	OCAMLPATH=`pwd`/../_build/bundle ocamlfind opt -rectypes -o sort -syntax camlp5o -package ocanren,ocanren.syntax,GT -linkpkg sort.ml
	OCAMLPATH=`pwd`/../_build/bundle ocamlfind opt -rectypes -o tree -syntax camlp5o -package ocanren,ocanren.syntax,GT,GT.syntax.all -linkpkg tree.ml

ppx_demo:
	OCAMLPATH=`pwd`/../_build/bundle ocamlfind ocamlopt -predicates ppx_driver \
		-o $(PPX_DRIVER_RUNNER) -linkpkg \
	  -package ppx_sexp_conv,ocanren.distrib \
	  -package ppx_driver.runner
	OCAMLPATH=`pwd`/../_build/bundle ocamlfind opt -rectypes \
		-ppx './$(PPX_DRIVER_RUNNER) --as-ppx' -o ppx_peano \
		-package ocanren,GT -linkpkg ppx_peano.ml

clean:
	$(RM) *.cm[oix] *.o sort tree
	$(RM) -f $(PPX_DRIVER_RUNNER) ppx_peano
