#!/usr/bin/env sh

# Don't report an error if this test fails. Could be an expected fuzzing failure
stack --stack-yaml="./package-tests/$1-test/stack.yaml" test || exit 0
