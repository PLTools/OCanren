#!/bin/bash

# simple script for promoting test results;
# please, run the script from the toplevel directory of the project

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

if [[ ! -f ${TESTLOG} ]]; then
    echo "cannot find ${TESTLOG}"
    exit 1
fi

cp ${TESTLOG} ${TESTORIG}