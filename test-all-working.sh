#!/usr/bin/env sh

set -euo pipefail

# Packags skipped for now:
# Agda - Strange parsing error
# Color - Probably do-able but setting up data is complicated
# adjunctions - Complicated class constraints
# aeson - GADTs
# algebraic graphs - GHC Doesn't support impredicative polymorphism
#                  - TODO Take another look at this one, has some interesting rules

# Packages w/o rules
# MemoTrie
# base-compat (no rules for this version)


./apply-vendored-patches.sh
./test.sh ListLike
./test.sh basement
./test.sh fast-math

# These tests are somewhat fragile, you may need to run multiple times
# to avoid errors (-10, -11). Not sure why they're happening
# Run this at the end because of these errors
./test.sh arithmoi

echo "All tests completed successfully"
