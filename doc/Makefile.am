# define macros that creates temporary files
if MINGW32
    MKTEMP=$(top_srcdir)/config/mktemp.sh
else
    MKTEMP=mktemp
endif

PARSER_INCLUDES += -I +camlp5

.PHONY: doc
# check if style.css already exists to avoid everytime generation
# use different preprocessors for OCaml and CamlP5 sources
doc: 
	test -d html || mkdir html
	dump1=`$(MKTEMP) ocamldoc.XXXXXX` && \
	dump2=`$(MKTEMP) ocamldoc.XXXXXX` && \
	dump3=`$(MKTEMP) ocamldoc.XXXXXX` && \
	$(OCAMLDOC) $(MODULE_INCLUDES) -short-functors -rectypes -dump "$$dump1" \
	    -I $(top_srcdir)/src $(top_srcdir)/src/*.mli  && \
	$(OCAMLDOC) -I +camlp5 $(MODULE_INCLUDES) $(P5FLAGS) -short-functors -rectypes -dump "$$dump2" \
	    -pp "$(CAMLP5O) $(PARSER_INCLUDES) " \
	    -I $(top_srcdir)/src -I $(top_srcdir)/camlp5 \
	    $(top_srcdir)/camlp5/*.ml && \
	$(OCAMLDOC) $(MODULE_INCLUDES) $(P5FLAGS) -short-functors -rectypes -dump "$$dump3" \
	    -pp "$(CAMLP5O) ../camlp5/pa_ostap.cmo " \
	    -I $(top_srcdir)/src -I $(top_srcdir)/camlp5 \
	    $(top_srcdir)/util/*.mli && \
	$(OCAMLDOC) -short-functors -load "$$dump1" -load "$$dump2" -load "$$dump3" -d html -html && \
	rm -f "$$dump1" "$$dump2" "$$dump3"
	if [ -f $(top_srcdir)/src/style.css ] ; then cp $(top_srcdir)/src/style.css html/ ; fi

# default clean
clean-am:
	rm -fR html/*.html html/*.css
