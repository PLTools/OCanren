.PHONY: celan \
	lib ppx syntax package \
	test promote discover-tests clean-test \
	regression samples clean-samples watch \
	install uninstall

.DEFAULT_GOAL: all

all: package

package: lib ppx syntax

lib:
	dune build src

ppx:
	dune build ppx

syntax:
	dune build camlp5/pa_ocanren.cma

regression:
	dune build regression

test: regression #samples
	dune test

promote:
	dune test --auto-promote

discover-tests:
	dune build @discover-tests
	# You can generate cram test files too using
	# dune exec config/discover.exe -- -tests -tests-dir regression -cram-files `realpath regression`

samples:
	dune build samples

celan: clean
clean:
	dune clean

watch:
	dune build -w camlp5/pa_ocanren.cma ppx/ src/

install:
	dune build @install
	dune install

uninstall:
	dune build @install
	dune uninstall
