MKDIR ?= mkdir -vp
CP    ?= cp

.PHONY: \
#	all clean install uninstall \
#	lib syntax package \
#	test tests samples promote promote-all clean-tests
	all clean \
	lib syntax package

.DEFAULT_GOAL: all

all: package samples

package: lib syntax

lib:
	dune build src/OCanren.cma
	dune build src/OCanren.cmxa

syntax:
	dune build camlp5/pa_ocanren.cma

clean:
	dune clean
