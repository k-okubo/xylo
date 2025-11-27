#!/bin/sh

set -e
set -x


find include src test -name '*.h' -o -name '*.cpp' | xargs clang-format -i --verbose
