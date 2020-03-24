.PHONY: celan \
	lib ppx syntax package \
	test promote discover-tests clean-test \
	samples clean-samples \
	install uninstall

.DEFAULT_GOAL: all

all: package samples

package: lib ppx syntax

lib:
	dune build src

ppx:
	dune build ppx

syntax:
	dune build camlp5/pa_ocanren.cma

test:
	./test.sh all

promote:
	./test.sh --promote all

discover-tests:
	dune build @discover-tests

samples:
	dune build samples

clean-test:
	rm -f regression/*.log regression/*.diff

clean-samples:
	rm -f samples/*.log samples/*.diff

celan: clean
clean: clean-test clean-samples
	dune clean

install:
	dune build @install
	dune install

uninstall:
	dune build @install
	dune uninstall
