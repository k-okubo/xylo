#!/bin/sh

set -e
set -x

BIN_DIR=$(dirname "$0")/build/bin


for f in $BIN_DIR/*_test; do
  if [ -x "$f" ]; then
    valgrind --tool=memcheck --leak-check=full --suppressions=memcheck.supp "$f" > /dev/null
    echo
  fi
done
