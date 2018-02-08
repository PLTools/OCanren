BUILD=ocamlbuild -use-ocamlfind
all:
	OCAMLPATH=`pwd`/../_build/bundle $(BUILD) sorting.native tree.native # lorry.native WGC.native

clean:
	$(RM) -r _build *.native
