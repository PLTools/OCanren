# type in sources here
SOURCES = mkshow.ml

include $(top_srcdir)/m4/Makefile.am.vars

CLEAN_EXTRA += 
DEBUG = -g

# overwrite some variables
PXFLAGS = $(P5FLAGS)
CAMLPXLIB = $(CAMLP5LIB)
MODULE_INCLUDES += -I $(top_srcdir)/src -I $(top_srcdir)/camlp5 -I "$(CAMLP5LIB)"

include $(top_srcdir)/m4/Makefile.am.targets

# overwrite some targets
TARGET = $(CMO) 

.PHONY: doc
doc:

include $(top_srcdir)/m4/Makefile.am.rules
