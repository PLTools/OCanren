#!/bin/sh

find . -type f -name Makefile -print0 | xargs -r0 rm -f
find . -type f -name Makefile.in -print0 | xargs -r0 rm -f
rm -f config.* configure aclocal.m4
rm -f config/config.* config/install-sh config/missing config/mkinstalldirs
rm -fR autom4te.cache doc/html/*
