# type in sources here
SOURCES = pa_minikanren.ml

include $(top_srcdir)/m4/Makefile.am.vars

AM_BFLAGS += -w x -rectypes
AM_OFLAGS += -w x -rectypes

# overwrite some variables
PXFLAGS = $(P5FLAGS)
CAMLPXLIB = $(CAMLP5LIB)
MODULE_INCLUDES += -I $(top_srcdir)/src -I $(CAMLP5LIB)

include $(top_srcdir)/m4/Makefile.am.targets

# overwrite some targets
TARGET = $(CMO)

include $(top_srcdir)/m4/Makefile.am.rules
