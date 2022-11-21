#!/usr/bin/env sh

stack run -- "$1" || exit 1

grep "$1-test" stack.yaml > /dev/null || echo "- package-tests/$1-test" >> stack.yaml

# Don't report an error if this test fails. Could be an expected fuzzing failure
stack test "$1-test" || exit 0
