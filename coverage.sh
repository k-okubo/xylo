#!/bin/bash

set -e


BASE_DIR=$(dirname "$0")
BINARY_DIR=$BASE_DIR/build

PROF_DIR=$BINARY_DIR/prof
COV_DIR=$BINARY_DIR/cov


if [ ! -d "$BINARY_DIR" ]; then
  echo "Build directory not found: $BINARY_DIR"
  exit 1
fi

rm -rf $PROF_DIR && mkdir $PROF_DIR
rm -rf $COV_DIR && mkdir $COV_DIR


COMPILER_PATH=$(cmake -LA -N -B build | awk -F= '/^CMAKE_CXX_COMPILER:/{print $2}')

if [[ "$COMPILER_PATH" == *"clang++"* ]]; then
  COMPILER_ID="Clang"
elif [[ "$COMPILER_PATH" == *"g++"*  ]]; then
  COMPILER_ID="GNU"
else
  echo "Unsupported compiler for coverage: $COMPILER_PATH"
  exit 1
fi


cmake --build $BINARY_DIR -j

case "$COMPILER_ID" in
  "Clang")
    LLVM_PROFILE_FILE="$(realpath $PROF_DIR)/%p_%m.profraw" ctest --test-dir $BINARY_DIR --output-on-failure

    llvm-profdata merge -sparse $PROF_DIR/*.profraw -o $COV_DIR/coverage.profdata

    llvm-cov export -format=lcov \
      -instr-profile=$COV_DIR/coverage.profdata \
      $(find $BINARY_DIR/bin -type f -name "*test" | xargs -I {} echo -object {}) \
      > $COV_DIR/lcov.info

    llvm-cov show -format=html \
      -output-dir=$COV_DIR/html \
      -instr-profile=$COV_DIR/coverage.profdata \
      $(find $BINARY_DIR/bin -type f -name "*test" | xargs -I {} echo -object {})
    ;;

  "GNU")
    ctest --test-dir $BINARY_DIR --output-on-failure
    lcov --capture --directory $BINARY_DIR --output-file $COV_DIR/lcov.info --ignore-errors mismatch
    genhtml $COV_DIR/lcov.info --output-directory $COV_DIR/html
    ;;
esac
