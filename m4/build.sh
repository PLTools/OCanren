#!/bin/sh

# This script checks required OCaml modules in build directory,
# downloads and builds missing.

# To mark module to check use "dnl GET:" syntax in configure.in :
#AC_CHECK_OCAML_MODULE(libname, Module1 Module2, ../libdir, http://www.example.com/libname) dnl GET: svn co svn://www.example.com/svn/libname/trunk
# The same for AC_CHECK_CAMLP4_MODULE and AC_CHECK_CAMLP5_MODULE.

OLDPWD=`pwd`
cd `dirname $0`
for keyword in AC_CHECK_CAMLP4_MODULE AC_CHECK_CAMLP5_MODULE AC_CHECK_OCAML_MODULE AC_CHECK_OCAML_LIBRARY; do
	grep $keyword configure.in | \
	grep "dnl GET" | \
	while read line; do
		module=${line#$keyword(}
		module=${module%%,*}
		command=${line#*dnl GET: }
		test "$command" = "$line" && continue

		test -d "../$module" || eval "$command ../$module"
		
		"../$module/build.sh" || break
	done
done

test -x configure || ./autogen.sh
test -f Makefile || ./configure
make
cd "$OLDPWD"
