MKDIR ?= mkdir -vp
CP    ?= cp

#	all clean install uninstall \
#	lib syntax package \
#	test tests samples promote promote-all clean-tests

.PHONY: \
	all clean \
	lib syntax package \
	samples clean-samples

.DEFAULT_GOAL: all

all: package samples

package: lib syntax

lib:
	dune build src/OCanren.cma
	dune build src/OCanren.cmxa

syntax:
	dune build camlp5/pa_ocanren.cma

samples:
	dune build @samples

clean-samples:
	rm -f samples/*.log samples/*.diff

clean: clean-samples
	dune clean