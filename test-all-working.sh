#!/usr/bin/env sh

set -euo pipefail

./apply-vendored-patches.sh
./test.sh basement
./test.sh fast-math

# These tests are somewhat fragile, you may need to run multiple times
# to avoid errors (-10, -11). Not sure why they're happening
./test.sh arithmoi

# ./test.sh base-compat
