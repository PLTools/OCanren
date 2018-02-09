all:
	OCAMLPATH=`pwd`/../_build/bundle ocamlfind opt -rectypes -o sort -syntax camlp5o -package ocanren,ocanren.syntax,GT -linkpkg sort.ml
	OCAMLPATH=`pwd`/../_build/bundle ocamlfind opt -rectypes -o tree -syntax camlp5o -package ocanren,ocanren.syntax,GT,GT.syntax.all -linkpkg tree.ml

clean:
	$(RM) *.cm[oix] *.o sort tree
