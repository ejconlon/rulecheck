#!/usr/bin/env sh

set -euo pipefail

stack run -- "$1"

# Just compile the tests, this will signal an error if compilation fails
stack --stack-yaml="./package-tests/$1-test/stack.yaml" test --no-run-tests

./fuzz.sh "$1"
