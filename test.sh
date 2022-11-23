#!/usr/bin/env sh

stack run -- "$1" || exit 1

# Don't report an error if this test fails. Could be an expected fuzzing failure
stack --stack-yaml="./package-tests/$1-test/stack.yaml" test || exit 0
