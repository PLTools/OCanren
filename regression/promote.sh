#!/bin/bash

# simple script for promoting test results;
# please, run the script from the toplevel directory of the project

if test $# = 0; then
    echo "usage: $0 <test name>"
    exit 1
fi

TEST=$1
TESTDIR="regression"
TESTLOG="${TESTDIR}/${TEST}.log"
TESTORIG="${TESTDIR}/orig/${TEST}.orig"

if [[ ! -f ${TESTLOG} ]]; then
    echo "ERROR: could not find ${TESTLOG}"
    exit 1
fi

cp ${TESTLOG} ${TESTORIG}