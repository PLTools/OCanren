BUILD=ocamlbuild -use-ocamlfind
all:
	OCAMLPATH=`pwd`/../_build/bundle $(BUILD) Sorting.native tree.native JeepProblem.native
#WGC.native

clean:
	$(RM) -r _build *.native
