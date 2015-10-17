CAMLP5PP=camlp5o pa_gt.cmo -L `ocamlfind query GT`
OCAML_OPTS=-rectypes -g
OCAMLOPT=ocamlfind opt $(OCAML_OPTS) -package typeutil,camlp5,GT -pp $(CAMLP5PP)
OCAMLC=ocamlfind c $(OCAML_OPTS) -package typeutil,camlp5,GT -pp "$(CAMLP5PP)"
TEST_OUT=test.byte
.SUFFIXES: .cmo .cmx .ml
CMOS=Stream.cmo MiniKanren.cmo test.cmo
BINDIR=$(shell opam config var bin)
# camlp5o pr_o.cmo pa_gt.cmo -L /home/kakadu/.opam/4.01.0/lib/camlp5 -L . test.ml

all: minikanren.cmo $(CMOS) $(TEST_OUT)

test.cmo: CAMLP5PP += -L `camlp5 -where` -L .

.ml.cmo:
	$(OCAMLC) -c $<


$(TEST_OUT): CAMLP5PP :=

$(TEST_OUT): $(CMOS)
	$(OCAMLC) -package camlp5,typeutil,dynlink,GT -linkpkg $^ -o $@

clean:
	$(RM) *.cm[ioxa] *.o $(TEST_OUT)

install: $(TEST_OUT)
	install -m 644 $(TEST_OUT) $(BINDIR)/minikanren_test

uninstall:
	$(RM) $(BINDIR)/minikanren_test

# depends
test.cmo: MiniKanren.cmo Stream.cmo
MiniKanren.cmo: Stream.cmo
