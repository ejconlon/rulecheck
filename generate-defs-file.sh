#!/usr/bin/env sh
#
# Note: Tested using Cabal version 3.8.1.0
# Older versions may not work!!!

set -euo pipefail

HADDOCK=$(which haddock)

HOOGLE_FILE=$(cabal haddock --with-haddock=$HADDOCK --with-ghc=ghc-9.0.2 --haddock-hoogle | tail -n1)

# sed '/-> \*/d' is a heuristic for eliminating things w/ higher kinded types
# sed '/forall.*forall/d eliminates stuff w/ nested foralls
# The command sed 's/^\[\(.*\)\]/\1/g' removes replaces [ID] with ID for haddock hidden syntax
sed '/^--/d' "$HOOGLE_FILE" | \
    sed '/-> \*/d'|\
    sed '/forall.*forall/d'|\
    sed 's/^newtype /data /' | sed 's/^\[\(.*\)\]/\1/g' > defs.txt
