#!/usr/bin/env sh

set -euo pipefail

# Packages skipped for now:
# Agda - Strange parsing error
# Color - Probably do-able but setting up data is complicated
# adjunctions - Complicated class constraints
# aeson - GADTs
# algebraic graphs - GHC Doesn't support impredicative polymorphism
#                  - TODO Take another look at this one, has some interesting rules
# These packages don't work for synth:
# bifunctors - rules only for higher-kinded types it seems
# binary-list - cannot introduce type vars
# bits - higher-kinded datatype breaks parsing of defs file
# boring - can't handle default class (must update haddock fork)
# brick - missingcon likely due to hk datatype, but this one looks hard
# byteable - no args, but WE CAN DO THIS ONE EASILY MANUALLY
# bytestring-builder - haddock docs not building for some reason
# bytestring-strict-builder - doesn't know how to build Bytestring (different lib). WE CAN DO EASILY MANUALLY
# bytestring-trie - doens't know how to generate ptr, parsing can't handle apostraphe
# carray - no rules have args
# case-insensitive -m no rules have args (SHOULD BE EASY MANUALLY, bytestring)
# cassava - looks to be another default issue
# clashLib - Core.Var.varType is both a type and a kind? this one's prob. too complicated

# Packages w/o rules
# MemoTrie
# base-compat (no rules for this version)
# blaze-builder (only rules in test)


./apply-vendored-patches.sh
./test.sh ListLike
./test.sh basement
./test.sh fast-math

# These tests are somewhat fragile, you may need to run multiple times
# to avoid errors (-10, -11). Not sure why they're happening
# Run this at the end because of these errors
./test.sh arithmoi

echo "All tests completed successfully"
