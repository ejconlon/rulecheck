#!/usr/bin/env sh

set -euo pipefail

# Known Packages with issues:
# Agda - Strange parsing error
# Color - Probably do-able but setting up data is complicated
# #


./apply-vendored-patches.sh
./test.sh ListLike
./test.sh basement
./test.sh fast-math

# These tests are somewhat fragile, you may need to run multiple times
# to avoid errors (-10, -11). Not sure why they're happening
# Run this at the end because of these errors
./test.sh arithmoi

# ./test.sh base-compat

echo "All tests completed successfully"
