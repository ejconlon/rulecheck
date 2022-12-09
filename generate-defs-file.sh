#!/usr/bin/env sh
#
# Note: Tested using Cabal version 3.8.1.0
# Older versions may not work!!!

set -euo pipefail
set -x

HADDOCK=$(which haddock)

# See https://gitlab.haskell.org/ghc/ghc/-/issues/20592#note_391266
if [[ $OSTYPE == 'darwin'* ]]; then
    export C_INCLUDE_PATH=$(xcrun --show-sdk-path)/usr/include/ffi
fi
cabal haddock -j --with-haddock=$HADDOCK --with-ghc=ghc-9.0.2 --haddock-hoogle | tee log.txt

HOOGLE_FILE=$(tail -n1 log.txt)

# sed '/-> \*/d' is a heuristic for eliminating things w/ higher kinded types
# sed '/forall.*forall/d eliminates stuff w/ nested foralls
# The command sed 's/^\[\(.*\)\]/\1/g' removes replaces [ID] with ID for haddock hidden syntax
sed '/^--/d' "$HOOGLE_FILE" | \
    sed '/-> \*/d'|\
    sed '/forall.*forall/d'|\
    sed 's/^data /type /' |\
    sed 's/{-# UNPACK #-}//' |\
    sed 's/!//g' |\
    sed 's/^newtype /type /' | sed 's/^\[\(.*\)\]/\1/g' > defs.txt
