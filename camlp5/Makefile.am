# type in sources here
SOURCES = $(top_srcdir)/src/bNF3.ml extension.ml
SUBDIRS = regression 

include $(top_srcdir)/m4/Makefile.am.vars

AM_BFLAGS += -w x -rectypes
AM_OFLAGS += -w x -rectypes

CMOPK = pa_ostap.cmo
CLEAN_EXTRA += pa_ostap.cmi

# overwrite some variables
PXFLAGS = $(P5FLAGS)
CAMLPXLIB = $(CAMLP5LIB)
MODULE_INCLUDES += -I $(top_srcdir)/src -I $(CAMLP5LIB)

include $(top_srcdir)/m4/Makefile.am.targets

# overwrite some targets
TARGET = $(CMOPK)

include $(top_srcdir)/m4/Makefile.am.rules
