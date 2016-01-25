TESTS = test000 test001 test002 test003 test004 test005 test006 test007 test008 test009 test010 test011 test012

TESTS_ENVIRONMENT = $(srcdir)/test.sh

include $(top_srcdir)/m4/Makefile.am.vars

PARSER_INCLUDES += -I $(top_srcdir)/camlp5
MODULE_INCLUDES += -I $(top_srcdir)/src

EXTRA_CMA += $(PACKAGE).cma

# OCaml parser call with flags
AM_P5FLAGS = -pp "$(CAMLP5O) $(PARSER_INCLUDES) pr_o.cmo pa_log.cmo pa_minikanren.cmo pa_gt.cmo -L `camlp5o -where`" tester.ml

# overwrite some variables
PXFLAGS = $(P5FLAGS)
CAMLPXLIB = $(CAMLP5LIB)
MODULE_INCLUDES += -I $(CAMLP5LIB)

BFLAGS += -rectypes
OFLAGS += -rectypes

TARGET = 

CLEAN_EXTRA += $(addsuffix .cmi, $(TESTS)) $(addsuffix .cmo, $(TESTS))
CLEAN_EXTRA += $(addsuffix $(EXEEXT), $(TESTS))
CLEAN_EXTRA += *.diff *.log

include $(top_srcdir)/m4/Makefile.am.targets

# do not install anyting
install-data-am:
	@true
uninstall-am:
	@true
# do not call ocamldep
.depend:
	touch .depend

# Rules to build tests
%: %.ml
	$(OCAMLC) $(BFLAGS) $(EXTRA_CMA) $(PXFLAGS) -o $@$(EXEEXT) $<

include $(top_srcdir)/m4/Makefile.am.rules
