#!/bin/bash

set -x
pushd `dirname $0`
autoreconf -fi
popd
