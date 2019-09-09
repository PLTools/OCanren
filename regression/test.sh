#!/bin/bash

# simple script for running test and diffing its result with the expected;
# please, run the script from the toplevel directory of the project

set -e

if test $# = 0; then
    echo "usage: $0 <test name>"
    exit 1
fi

TEST=$1
TESTDIR="regression"
TESTEXE="${TESTDIR}/${TEST}.exe"
TESTLOG="${TESTDIR}/${TEST}.log"
TESTDIFF="${TESTDIR}/${TEST}.diff"
TESTORIG="${TESTDIR}/orig/${TEST}.orig"

# run the test;
# we use `dune exec` so that `dune`
# can build the test and its dependencies
# if it has not been done

dune exec --no-print-directory ${TESTEXE} > ${TESTLOG}

# diff test output with the expected and print `PASSED/FAILED` message

if diff -u ${TESTORIG} ${TESTLOG} > ${TESTDIFF}; then
    echo "${TEST}: PASSED"
    rm -f ${TESTDIFF}
    exit 0
else
    echo "${TEST}: FAILED (see ${TESTDIFF})"
    exit 1
fi