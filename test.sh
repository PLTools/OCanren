#!/bin/bash

# simple script for running test and diffing its result with the expected;
# please, run the script from the toplevel directory of the project

set -e

if test $# = 0; then
    echo "usage: $0 <test name>"
    exit 1
fi

TEST=$1

TESTDIRS="regression samples"

FOUND=0

for DIR in ${TESTDIRS}; do
  TESTDIR=${DIR}
  TESTSRC="${TESTDIR}/${TEST}.ml"
  if [[ -e ${TESTSRC} ]]; then
    FOUND=1
    break
  fi
done

if [[ ${FOUND} -eq "0" ]]; then
  echo "cannot find ${TEST}"
  echo "searched in ${TESTDIRS}"
  exit
fi

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