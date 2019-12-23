#!/bin/bash

# simple script for running test and diffing its result with the expected;
# please, run the script from the toplevel directory of the project

# predefined directory with tests
TESTDIR="regression"

# path to the file containing list of tests
TESTLIST="${TESTDIR}/tests.txt"

# directories to search for tests
SEARCH_DIRS="${TESTDIR} samples"

PROMOTE=0
ARG=""

USAGE_MSG="usage: $0 [--promote] (<test name> | all)"

if test $# = 0; then
    echo "${USAGE_MSG}"
    exit 1
fi

if [[ $1 == "--promote" ]]; then
  if test $# = 1; then
    echo "${USAGE_MSG}"
    exit 1
  fi
  PROMOTE=1
  ARG=$2
else
  ARG=$1
fi

if [[ ${ARG} == "all" ]]; then
  # read test names from the file
  TESTS=`cat ${TESTLIST}`
else
  # try to find test/sample in predefined directories
  FOUND=0

  for DIR in ${SEARCH_DIRS}; do
    TESTDIR=${DIR}
    TESTSRC="${TESTDIR}/${ARG}.ml"
    if [[ -f ${TESTSRC} ]]; then
      FOUND=1
      break
    fi
  done

  if [[ ${FOUND} -eq "0" ]]; then
    echo "cannot find ${ARG}"
    echo "searched in ${SEARCH_DIRS}"
    exit
  fi

  TESTS=${ARG}

fi

for TEST in ${TESTS}; do

  TESTEXE="${TESTDIR}/${TEST}.exe"
  TESTLOG="${TESTDIR}/${TEST}.log"
  TESTDIFF="${TESTDIR}/${TEST}.diff"
  TESTORIG="${TESTDIR}/orig/${TEST}.orig"

  # if the `--promote` option set, perform the promotion
  if [[ ${PROMOTE} -eq "1" ]]; then
    if [[ ! -f ${TESTLOG} ]]; then
      echo "cannot find ${TESTLOG}"
    else
      cp ${TESTLOG} ${TESTORIG}
    fi
    continue
  fi

  # run the test;
  # we use `dune exec` so that `dune`
  # can build the test and its dependencies
  # if it has not been done yet

  dune exec --no-print-directory ${TESTEXE} > ${TESTLOG}

  # diff test output with the expected and print `PASSED/FAILED` message

  if diff -u ${TESTORIG} ${TESTLOG} > ${TESTDIFF}; then
      echo "${TEST}: PASSED"
      rm -f ${TESTDIFF}
  else
      echo "${TEST}: FAILED (see ${TESTDIFF})"
  fi

done