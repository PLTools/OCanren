dnl This file was synchronized with template ($Revision: 65 $)
dnl
dnl -*- autoconf -*- macros for OCaml
dnl by Grigory Batalov <bga@altlinux.org>, 2005
dnl by Olivier Andrieu
dnl from a configure.in by Jean-Christophe FilliUtre,
dnl from a first script by Georges Mariano
dnl
dnl defines AC_PROG_OCAML that will check the OCaml compiler
dnl and set the following variables :
dnl   OCAMLC        "ocamlc" if present in the path, or a failure
dnl                 or "ocamlc.opt" if present with same version number as ocamlc
dnl   OCAMLOPT      "ocamlopt" (or "ocamlopt.opt" if present), or unset
dnl   OCAMLDEP      "ocamldep"
dnl   OCAMLLIB      the path to the ocaml standard library
dnl   OCAMLVERSION  the ocaml version number
dnl
dnl   OCAMLMKTOP    
dnl   OCAMLMKLIB    
dnl   OCAMLDOC

AC_DEFUN([AC_PROG_OCAML], [

dnl Checking for OCaml compiler
AC_CHECK_PROG(have_ocamlc, ocamlc, yes, no)
if test "$have_ocamlc" = no; then
    AC_MSG_ERROR(Cannot find ocamlc)
    unset OCAMLC
else
    OCAMLC=ocamlc
fi

dnl Checking OCaml version
AC_MSG_CHECKING(for OCaml version)
OCAMLVERSION=$($OCAMLC -version)
AC_MSG_RESULT($OCAMLVERSION)
if test "$OCAMLVERSION" = ""; then
    AC_MSG_ERROR(OCaml version not set)
fi

dnl Searching for library path
AC_MSG_CHECKING(for OCaml library path)
OCAMLLIB=$($OCAMLC -where)
AC_MSG_RESULT($OCAMLLIB)
if test "$OCAMLLIB" = ""; then
    AC_MSG_ERROR(OCaml library path)
fi

dnl Checking for OCaml native compiler
AC_CHECK_PROG(have_ocamlopt, ocamlopt, yes, no)
if test "$have_ocamlopt" = "no"; then
    AC_MSG_WARN(Cannot find ocamlopt; bytecode compilation only)
    unset OCAMLOPT
else
    OCAMLOPT=ocamlopt
    AC_MSG_CHECKING(for ocamlopt version)
    TMPVERSION=$($OCAMLOPT -version)
    AC_MSG_RESULT($TMPVERSION)
    if test "$TMPVERSION" = ""; then
	AC_MSG_ERROR(ocamlopt version not set; ocamlopt discarded)
	unset OCAMLOPT
    fi
    if test "$TMPVERSION" != "$OCAMLVERSION" ; then
	AC_MSG_WARN(($TMPVERSION) differs from ocamlc; ocamlopt discarded)
	unset OCAMLOPT
    fi
fi

dnl Checking for ocamlc.opt
AC_CHECK_PROG(have_ocamlcdotopt, ocamlc.opt, yes, no)
if test "$have_ocamlcdotopt" = "no"; then
    unset OCAMLCDOTOPT
else
    OCAMLCDOTOPT=ocamlc.opt
    AC_MSG_CHECKING(for ocamlc.opt version)
    TMPVERSION=$($OCAMLCDOTOPT -version)
    AC_MSG_RESULT($TMPVERSION)
    if test "$TMPVERSION" = ""; then
	AC_MSG_ERROR(ocamlc.opt version not set; ocamlc.opt discarded)
	unset OCAMLCDOTOPT
    fi
    if test "$TMPVERSION" != "$OCAMLVERSION" ; then
	AC_MSG_RESULT(($TMPVERSION) differs from ocamlc; ocamlc.opt discarded)
	unset OCAMLCDOTOPT
    else
	OCAMLC=$OCAMLCDOTOPT
    fi
fi

dnl Checking for ocamlopt.opt
if test "$have_ocamlopt" = "yes"; then
    AC_CHECK_PROG(have_ocamloptdotopt, ocamlopt.opt, yes, no)
    if test "$OCAMLOPTDOTOPT" = "no"; then
	unset OCAMLOPTDOTOPT
    else
	OCAMLOPTDOTOPT=ocamlopt.opt
	AC_MSG_CHECKING(for ocamlopt.opt version)
	TMPVERSION=$($OCAMLOPTDOTOPT -version)
	AC_MSG_RESULT($TMPVERSION)
	if test "$TMPVERSION" = ""; then
	    AC_MSG_ERROR(ocamlopt.opt version not set; ocamlopt.opt discarded)
	    unset OCAMLOPTDOTOPT
	fi
	if test "$TMPVERSION" != "$OCAMLVERSION" ; then
	    AC_MSG_RESULT(($TMPVERSION) differs from ocamlc; ocamlopt.opt discarded)
	    unset OCAMLOPTDOTOPT
	else
	    OCAMLOPT=$OCAMLOPTDOTOPT
	fi
    fi
fi

dnl Checking for ocamldep
AC_CHECK_PROG(have_ocamldep, ocamldep, yes, no)
if test "$have_ocamldep" = no; then
    AC_MSG_ERROR(Cannot find ocamldep)
    unset OCAMLDEP
else
    OCAMLDEP=ocamldep
fi

dnl Checking for ocamlmktop
AC_CHECK_PROG(have_ocamlmktop, ocamlmktop, yes, no)
if test "$have_ocamlmktop" = no; then
    AC_MSG_WARN(Cannot find ocamlmktop)
    unset OCAMLMKTOP
else
    OCAMLMKTOP=ocamlmktop
fi

dnl Checking for ocamlmklib
AC_CHECK_PROG(have_ocamlmklib, ocamlmklib, yes, no)
if test "$have_ocamlmklib" = no; then
    AC_MSG_WARN(Cannot find ocamlmklib)
    unset OCAMLMKLIB
else
    OCAMLMKLIB=ocamlmklib
fi

dnl Checking for ocamldoc
AC_CHECK_PROG(have_ocamldoc, ocamldoc, yes, no)
if test "$have_ocamldoc" = no; then
    AC_MSG_WARN(Cannot find ocamldoc)
    unset OCAMLDOC
else
    OCAMLDOC=ocamldoc
fi

dnl Get the C compiler used by ocamlc
if test "$have_ocamlopt" ; then
    AC_MSG_CHECKING(for OCaml C compiler)
    touch conftest.c
    OCAMLCC=$($OCAMLC -verbose conftest.c 2>&1 | awk '/^\+/ {print $[]2 ; exit}')
    AC_MSG_RESULT($OCAMLCC)
    AC_CHECK_PROG(have_ocamlcc, $OCAMLCC, yes, no)
    if test "$have_ocamlcc" = "no"; then
	AC_MSG_WARN(Cannot find OCaml C compiler ($OCAMLCC); bytecode compilation only)
	unset OCAMLOPT
    fi
    rm -f conftest.c
fi

dnl Determine executable extension
OLDCC=$CC
CC=$OCAMLC
touch conftest.ml
ac_ext=ml
ac_link="$CC conftest.ml && ( rm -f conftest.cm?; test -f camlprog.exe && mv camlprog.exe a.exe || true )"
_AC_COMPILER_EXEEXT_DEFAULT
_AC_COMPILER_EXEEXT_WORKS
_AC_COMPILER_EXEEXT_O
rm -f a.out a.exe
CC=$OLDCC
unset OLDCC
AC_SUBST([EXEEXT], [$ac_cv_exeext])dnl

m4_pattern_allow([^AM_BFLAGS$])
m4_pattern_allow([^AM_OFLAGS$])
BFLAGS='$(AM_BFLAGS)'
OFLAGS='$(AM_OFLAGS)'

AC_SUBST(OCAMLC)
AC_SUBST(BFLAGS)
AC_SUBST(OFLAGS)
AC_SUBST(OCAMLVERSION)
AC_SUBST(OCAMLLIB)
AC_SUBST(OCAMLOPT)
AC_SUBST(OCAMLDEP)
AC_SUBST(OCAMLMKTOP)
AC_SUBST(OCAMLMKLIB)
AC_SUBST(OCAMLDOC)
])

dnl
dnl macro AC_PROG_OCAML_TOOLS will check OCamllex and OCamlyacc :
dnl   OCAMLLEX      "ocamllex" or "ocamllex.opt" if present
dnl   OCAMLYACC     "ocamlyac"
AC_DEFUN([AC_PROG_OCAML_TOOLS], [

dnl Checking for ocamllex
AC_CHECK_PROG(have_ocamllex, ocamllex, yes, no)
if test "$have_ocamllex" = no; then
    AC_MSG_ERROR(Cannot find ocamllex)
    unset OCAMLLEX
else
    OCAMLLEX=ocamllex
    AC_CHECK_PROG(have_ocamllexdotopt, ocamllex.opt, yes, no)
    if test "$have_ocamllexdotopt" = "no"; then
	unset OCAMLLEXDOTOPT
    else
	OCAMLLEXDOTOPT=ocamllex.opt
	OCAMLLEX=$OCAMLLEXDOTOPT
    fi
fi

dnl Checking for ocamlyacc
AC_CHECK_PROG(have_ocamlyacc, ocamlyacc, yes, no)
if test "$have_ocamlyacc" = no; then
    AC_MSG_ERROR(Cannot find ocamlyacc)
    unset OCAMLYACC
else
    OCAMLYACC=ocamlyacc
fi

AC_SUBST(OCAMLLEX)
AC_SUBST(OCAMLYACC)
])

dnl
dnl macro AC_PROG_CAMLP4 will check Camlp4
dnl   CAMLP4		"camlp4"
dnl   CAMLP4O		"camlp4o"
dnl   CAMLP4OF		"camlp4of"
dnl   CAMLP4R		"camlp4r"
dnl   CAMLP4RF		"camlp4rf"
dnl   CAMLP4LIB		parser library path
AC_DEFUN([AC_PROG_CAMLP4], [
AC_REQUIRE([AC_PROG_OCAML])
dnl Checking for camlp4
AC_CHECK_PROG(have_camlp4, camlp4, yes, no)
if test "$have_camlp4" = no; then
    AC_MSG_WARN(Cannot find camlp4)
    unset CAMLP4
else
    CAMLP4=camlp4
    AC_MSG_CHECKING(for camlp4 version)
    TMPVERSION=$($CAMLP4 -version 2>&1)
    AC_MSG_RESULT($TMPVERSION)
    VERSION="$1"
    if test -n "$VERSION"; then
	if test "$TMPVERSION" = ""; then
	    AC_MSG_WARN(camlp4 discarded: unknown version)
	    unset CAMLP4
	else
dnl Check versions with dots stripped
	    TMPVERSION=`echo "$TMPVERSION" | tr -d .`
	    VERSION=`echo "$VERSION" | tr -d .`
	    if test "$TMPVERSION" -lt "$VERSION"; then
		AC_MSG_WARN(camlp4 discarded: version too old (need $VERSION))
		unset CAMLP4
	    fi
	fi
    fi
fi

if test -n "$CAMLP4"; then

dnl Checking for Camlp4o
        AC_CHECK_PROG(have_camlp4o, camlp4o, yes, no)
	if test "$have_camlp4o" = no; then
	    unset CAMLP4O
	else
	    CAMLP4O=camlp4o
	fi
dnl Checking for Camlp4of
        AC_CHECK_PROG(have_camlp4of, camlp4of, yes, no)
	if test "$have_camlp4of" = no; then
	    unset CAMLP4OF
	else
	    CAMLP4OF=camlp4of
	fi
dnl Checking for Camlp4r
	AC_CHECK_PROG(have_camlp4r, camlp4r, yes, no)
	if test "$have_camlp4r" = no; then
	    unset CAMLP4R
	else
	    CAMLP4R=camlp4r
	fi
dnl Checking for Camlp4rf
	AC_CHECK_PROG(have_camlp4rf, camlp4rf, yes, no)
	if test "$have_camlp4rf" = no; then
	    unset CAMLP4RF
	else
	    CAMLP4RF=camlp4rf
	fi

dnl Searching for parser library path
	AC_MSG_CHECKING(for CamlP4 library path)
	CAMLP4LIB=$($CAMLP4 -where)
	AC_MSG_RESULT($CAMLP4LIB)
	if test "$CAMLP4LIB" = ""; then
	    AC_MSG_ERROR(CamlP4 library path)
	fi
else
	have_camlp4=no
fi

m4_pattern_allow([^AM_P4FLAGS$])
P4FLAGS='$(AM_P4FLAGS)'

AC_SUBST(CAMLP4)
AC_SUBST(P4FLAGS)
AC_SUBST(CAMLP4O)
AC_SUBST(CAMLP4OF)
AC_SUBST(CAMLP4R)
AC_SUBST(CAMLP4RF)
AC_SUBST(CAMLP4LIB)
])

dnl
dnl macro AC_PROG_CAMLP5 will check Camlp5
dnl   CAMLP5		"camlp5"
dnl   CAMLP5O		"camlp5o"
dnl   CAMLP5R		"camlp5r"
dnl   CAMLP5LIB		parser library path
AC_DEFUN([AC_PROG_CAMLP5], [
AC_REQUIRE([AC_PROG_OCAML])
dnl Checking for camlp5
AC_CHECK_PROG(have_camlp5, camlp5, yes, no)
if test "$have_camlp5" = no; then
    AC_MSG_WARN(Cannot find camlp5)
    unset CAMLP5
else
    CAMLP5=camlp5
    AC_MSG_CHECKING(for camlp5 version)
    TMPVERSION=$($CAMLP5 -v 2>&1)
    AC_MSG_RESULT($TMPVERSION)
    if test "$TMPVERSION" = ""; then
	AC_MSG_WARN(camlp5 version not set; camlp5 discarded)
	unset CAMLP5
    fi
fi

if test -n "$CAMLP5"; then

dnl Checking for Camlp5o
        AC_CHECK_PROG(have_camlp5o, camlp5o, yes, no)
	if test "$have_camlp5o" = no; then
	    unset CAMLP5O
	else
	    CAMLP5O=camlp5o
	fi
dnl Checking for Camlp5r
	AC_CHECK_PROG(have_camlp5r, camlp5r, yes, no)
	if test "$have_camlp5r" = no; then
	    unset CAMLP5R
	else
	    CAMLP5R=camlp5r
	fi

dnl Searching for parser library path
	AC_MSG_CHECKING(for CamlP5 library path)
	CAMLP5LIB=$($CAMLP5 -where)
	AC_MSG_RESULT($CAMLP5LIB)
	if test "$CAMLP5LIB" = ""; then
	    AC_MSG_ERROR(CamlP5 library path)
	fi
else
	have_camlp5=no
fi

m4_pattern_allow([^AM_P5FLAGS$])
P5FLAGS='$(AM_P5FLAGS)'

AC_SUBST(CAMLP5)
AC_SUBST(P5FLAGS)
AC_SUBST(CAMLP5O)
AC_SUBST(CAMLP5R)
AC_SUBST(CAMLP5LIB)
])

dnl
dnl macro AC_PROG_FINDLIB will check for the presence of ocamlfind
dnl disable by configure call with --without-findlib
dnl   OCAMLFIND		"ocamlfind"
AC_DEFUN([AC_PROG_FINDLIB], [
AC_ARG_WITH(findlib,[  --without-findlib       do not use findlib package system],
  skip_findlib="$withval")
dnl Checking for ocamlfind
if ! test "$skip_findlib" = no ; then 
	AC_CHECK_PROG(have_ocamlfind, ocamlfind, yes, no)
	if test "$have_ocamlfind" = no; then
	    AC_MSG_WARN(ocamlfind not found)
	    unset OCAMLFIND
	else
	    OCAMLFIND=ocamlfind
	fi
else
	unset OCAMLFIND
fi
AC_SUBST(OCAMLFIND)
])

dnl
dnl macro AC_PROG_OCAMLDSORT will check OCamlDSort
dnl   OCAMLDSORT    "ocamldsort"
AC_DEFUN([AC_PROG_OCAMLDSORT], [
AC_ARG_WITH(ocamldsort,[  --with-ocamldsort       sort sources before build],
  use_ocamldsort="$withval")
dnl Checking for ocamldsort
if test "$use_ocamldsort" = yes; then
	AC_CHECK_PROG(have_ocamldsort, ocamldsort, yes, no)
	if test "$have_ocamldsort" = no; then
	    AC_MSG_WARN(Cannot find ocamldsort[,] using echo)
	    OCAMLDSORT=echo
	else
	    OCAMLDSORT=ocamldsort
	fi
else
	OCAMLDSORT=echo
fi
AC_SUBST(OCAMLDSORT)
])


dnl
dnl AC_ARG_OCAML_SITELIBR adds a --with-sitelib option
dnl   1 -> where to install OCaml packages
AC_DEFUN([AC_ARG_OCAML_SITELIB], [
AC_ARG_WITH(sitelib,[  --with-sitelib=DIR      specify OCaml installation directory],
    SITELIBDIR="$withval")
AC_SUBST(SITELIBDIR)
])

dnl
dnl ocaml_compile tries to compile a source file with given module
dnl   1 -> module to check
dnl   2 -> capitalized name to use with open
dnl   3 -> extra searching dirs
dnl   MODULE_INCLUDES	include options, i.e. "-I /path/to/dir"

ocaml_compile() {

cat > conftest.ml <<EOF
open $2
EOF
dnl unset META
unset found
dnl Check module "as is"
    if $OCAMLC -c -rectypes $MODULE_INCLUDES conftest.ml > /dev/null 2>&1 ; then
	result="found"
	found=yes
    else
        if test "$skip_findlib" != "yes" -a "$OCAMLFIND" != ""; then 
dnl Query package via ocamlfind
	    if check_inc=`$OCAMLFIND -query -i-format $1 2>/dev/null`; then
		META=`$OCAMLFIND -query $1`/META
		if $OCAMLC -c -rectypes $MODULE_INCLUDES $check_inc conftest.ml > /dev/null 2>&1 ; then
		    MODULE_INCLUDES="$MODULE_INCLUDES $check_inc"
		    result="adding $check_inc"
		    found=yes
		fi
	    fi
	fi
	if ! test "$found"; then
dnl Search through specified dirs
	    if test -n "$SITELIBDIR"; then
		dirs="$3 $SITELIBDIR/$1 $OCAMLLIB/$1"
	    else
		dirs="$3 $OCAMLLIB/$1"
	    fi
	    for check_dir in $dirs; do
		META="$check_dir/META"
	    	builddir=`pwd`
	    	case $check_dir in
dnl Absolute Unix/Windows path or "+dir" syntax
		    [[\\/+]]*) ;;
dnl Absolute Windows path starting with letter
		    [[[:alpha:]]]:\\*) ;;
		    *) check_dir="$builddir/$check_dir";;
		esac
		if $OCAMLC -c -rectypes -I $check_dir conftest.ml > /dev/null 2>&1 ; then
		    MODULE_INCLUDES="$MODULE_INCLUDES -I $check_dir"
		    result="adding -I $check_dir"
		    found=yes
		    break
		fi
	    done
	fi
    fi
}

dnl
dnl AC_CHECK_OCAML_MODULE looks for a modules in a given path
dnl   1 -> module to check
dnl   2 -> capitalized names to use with open
dnl   3 -> extra searching dirs
dnl   MODULE_INCLUDES	include options, i.e. "-I /path/to/dir"
dnl   EXTRA_CMA		extra libraries to link with
dnl   4 -> URL for module homepage
AC_DEFUN([AC_CHECK_OCAML_MODULE], [
for module in $2; do
    AC_MSG_CHECKING(for $1 ($module))

    unset META
    ocaml_compile "$1" "$module" "$3"

    if test "$found" ; then
	    AC_MSG_RESULT($result $1.cmo)
	    EXTRA_CMA="$EXTRA_CMA $1.cmo"
    else
	AC_MSG_RESULT(not found)
	if ! test -z "$4"; then
	    AC_MSG_WARN(Please[,] visit $4)
	fi
	AC_MSG_ERROR(Required module $module not found in library $1)
    fi
done
AC_SUBST(MODULE_INCLUDES)
AC_SUBST(EXTRA_CMA)
AC_SUBST(META)
])


dnl
dnl AC_CHECK_OCAML_LIBRARY looks for a modules in a given path
dnl   1 -> module to check
dnl   2 -> capitalized names to use with open
dnl   3 -> extra searching dirs
dnl   MODULE_INCLUDES	include options, i.e. "-I /path/to/dir"
dnl   EXTRA_CMA		extra libraries to link with
dnl   4 -> URL for module homepage
AC_DEFUN([AC_CHECK_OCAML_LIBRARY], [
for module in $2; do
    AC_MSG_CHECKING(for $1 ($module))

    unset META
    ocaml_compile "$1" "$module" "$3"
    if test "$found" ; then

cat > conftest.ml <<EOF
open $module
EOF
dnl Try to link with library
		unset CMA
		if test -f "$META"; then
		    CMA=`grep "archive(byte)" $META | sed -e 's,[[^=]]*= *"\(.*\)",\1,g' | sed -e ':a;N;$!ba;s/\n/ /g'`
		fi
		test -z "$CMA" && CMA="$1.cma"
		if ! $OCAMLC -o conftest -rectypes $MODULE_INCLUDES $CMA conftest.cmo > /dev/null 2>&1 ; then
		    AC_MSG_RESULT($CMA not found)
	    	    if ! test -z "$4"; then
			AC_MSG_WARN(Please[,] visit $4)
	    	    fi
		    AC_MSG_ERROR(Required library $1 not found)
		else
		    if ! echo "$EXTRA_CMA" | grep "$CMA" > /dev/null 2>&1 ; then
			EXTRA_CMA="$EXTRA_CMA $CMA"
		    fi
		    AC_MSG_RESULT($result)
		fi
    else
		AC_MSG_RESULT(not found)
		if ! test -z "$4"; then
		    AC_MSG_WARN(Please[,] visit $4)
		fi
		AC_MSG_ERROR(Required module $module not found in library $1)
    fi
done
AC_SUBST(MODULE_INCLUDES)
AC_SUBST(EXTRA_CMA)
AC_SUBST(META)
])


dnl
dnl AC_CHECK_CAMLP4_MODULE looks for a module in a given path
dnl   1 -> module to check
dnl   2 -> file name to use with load
dnl   3 -> extra searching dirs
dnl   PARSER_INCLUDES		include options, i.e. "-I /path/to/dir"
dnl   4 -> URL for module homepage
AC_DEFUN([AC_CHECK_CAMLP4_MODULE], [
AC_MSG_CHECKING(for $1 ($2))
cat > conftest.ml <<EOF
#load "$2";
EOF
dnl Check module "as is"
if $CAMLP4R $PARSER_INCLUDES conftest.ml > /dev/null 2>&1 ; then
    AC_MSG_RESULT(found)
else
    unset found
    if ! test "$use_findlib" = no ; then 
dnl Query package via ocamlfind
	if check_inc=`$OCAMLFIND query -i-format $1 2>/dev/null`; then
	    if $CAMLP4R $PARSER_INCLUDES $check_inc conftest.ml > /dev/null 2>&1 ; then
		AC_MSG_RESULT(adding $check_inc)
		PARSER_INCLUDES="$PARSER_INCLUDES $check_inc"
		found=yes
	    fi
	fi
    fi
    if ! test "$found"; then
dnl Search through specified dirs
	if test -n "$SITELIBDIR"; then
	    dirs="$3 $SITELIBDIR/camlp4 $OCAMLLIB/camlp4"
	else
	    dirs="$3 $OCAMLLIB/camlp4"
	fi
	for check_dir in $dirs; do
	    builddir=`pwd`
	    case $check_dir in
dnl Absolute Unix/Windows path or "+dir" syntax
		[[\\/+]]*) ;;
dnl Absolute Windows path starting with letter
		[[[:alpha:]]]:\\*) ;;
		*) check_dir="$builddir/$check_dir";;
	    esac
	    if $CAMLP4R -I $check_dir conftest.ml > /dev/null 2>&1 ; then
		found=yes
		break
	    fi
	done
	if test "$found" ; then
	    AC_MSG_RESULT(adding -I $check_dir)
	    PARSER_INCLUDES="$PARSER_INCLUDES -I $check_dir"
	else
	    AC_MSG_RESULT(not found)
	    if ! test -z "$4"; then
		AC_MSG_WARN(Please[,] visit $4)
	    fi
	    AC_MSG_ERROR(Required module $1 not found)
	fi
    fi
fi
AC_SUBST(PARSER_INCLUDES)
])


dnl
dnl AC_CHECK_CAMLP5_MODULE looks for a module in a given path
dnl   1 -> module to check
dnl   2 -> file name to use with load
dnl   3 -> extra searching dirs
dnl   PARSER_INCLUDES		include options, i.e. "-I /path/to/dir"
dnl   4 -> URL for module homepage
AC_DEFUN([AC_CHECK_CAMLP5_MODULE], [
AC_MSG_CHECKING(for $1 ($2))
cat > conftest.ml <<EOF
let _ = 3
EOF
dnl Check module "as is"
if $CAMLP5O $2 $PARSER_INCLUDES conftest.ml > /dev/null 2>&1 ; then
    AC_MSG_RESULT(found)
else
    unset found
    if ! test "$use_findlib" = no ; then 
dnl Query package via ocamlfind
	if check_inc=`$OCAMLFIND query -i-format $1 2>/dev/null`; then
	    if $CAMLP5R $PARSER_INCLUDES $check_inc conftest.ml > /dev/null 2>&1 ; then
		AC_MSG_RESULT(adding $check_inc)
		PARSER_INCLUDES="$PARSER_INCLUDES $check_inc"
		found=yes
	    fi
	fi
    fi
    if ! test "$found"; then
dnl Search through specified dirs
	if test -n "$SITELIBDIR"; then
	    dirs="$3 $SITELIBDIR/camlp5 $OCAMLLIB/camlp5"
	else
	    dirs="$3 $OCAMLLIB/camlp5"
	fi
	for check_dir in $dirs; do
	    builddir=`pwd`
	    case $check_dir in
dnl Absolute Unix/Windows path or "+dir" syntax
		[[\\/+]]*) ;;
dnl Absolute Windows path starting with letter
		[[[:alpha:]]]:\\*) ;;
		*) check_dir="$builddir/$check_dir";;
	    esac
	    if $CAMLP5R -I $check_dir conftest.ml > /dev/null 2>&1 ; then
		found=yes
		break
	    fi
	done
	if test "$found" ; then
	    AC_MSG_RESULT(adding -I $check_dir)
	    PARSER_INCLUDES="$PARSER_INCLUDES -I $check_dir"
	else
	    AC_MSG_RESULT(not found)
	    if ! test -z "$4"; then
		AC_MSG_WARN(Please[,] visit $4)
	    fi
	    AC_MSG_ERROR(Required module $1 not found)
	fi
    fi
fi
AC_SUBST(PARSER_INCLUDES)
])
