
.PHONY: \
	all clean \
	lib ppx syntax package \
	test promote discover-tests clean-tests \
	samples clean-samples

.DEFAULT_GOAL: all

all: package samples

package: lib ppx syntax

lib:
	dune build src

ppx:
	dune build ppx ppxnew

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

clean-tests:
	rm -f regression/*.log regression/*.diff

clean-samples:
	rm -f samples/*.log samples/*.diff

clean: clean-tests clean-samples
	dune clean