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
# boring - can't handle default class (must update haddock fork)
# brick - missingcon likely due to hk datatype constructor, but this one looks hard
# byteable - no args, but WE CAN DO THIS ONE EASILY MANUALLY
# bytestring-builder - haddock docs not building for some reason
# bytestring-trie - doens't know how to generate ptr, parsing can't handle apostraphe
# carray - no rules have args
# case-insensitive no rules have args (SHOULD BE EASY MANUALLY, bytestring)
# cassava - looks to be another default issue
# clashLib - Core.Var.varType is both a type and a kind? this one's prob. too complicated
# concise no rules have args (SHOULD BE EASY MANUALLY)
# conduit - hk datatype constructor
# cryptohash - (no need for synth. SEGFAULTS, but should be easy to fix?)
# csv-conduit - can't handle default class (must update haddock fork)
# cubicbezier - various type parse errors that seem reasonable
# equational-reasoning - hk datatype constructor
# extensible-effects - hk datatype constructor

# Packages w/o rules
# MemoTrie
# base-compat (no rules for this version)
# blaze-builder (only rules in test)


# Only run tests, skip build
if [[ "${1:-}" == "--skip-build" ]]; then
   TEST_SCRIPT="./fuzz.sh"
else
   TEST_SCRIPT="./test.sh"
fi
./apply-vendored-patches.sh
$TEST_SCRIPT ListLike
$TEST_SCRIPT basement
$TEST_SCRIPT bits
$TEST_SCRIPT bytestring-strict-builder
$TEST_SCRIPT fast-math

# These tests are somewhat fragile, you may need to run multiple times
# to avoid errors (-10, -11). Not sure why they're happening
# Run this at the end because of these errors
$TEST_SCRIPT arithmoi

echo "All tests completed successfully"
