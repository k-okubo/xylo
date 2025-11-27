#!/bin/sh

set -x


DEFAULT_OPTIONS="--recursive --linelength=120 --filter=-legal/copyright,-readability/todo"

cpplint --root=include $DEFAULT_OPTIONS include
echo

cpplint $DEFAULT_OPTIONS src
echo

cpplint $DEFAULT_OPTIONS test
echo
