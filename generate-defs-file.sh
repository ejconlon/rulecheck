#!/usr/bin/env sh

set -euo pipefail

HOOGLE_FILE=$(cabal haddock --haddock-hoogle | tail -n1)

# The command sed 's/^\[\(.*\)\]/\1/g' removes replaces [ID] with ID for haddock hidden syntax
sed '/^--/d' "$HOOGLE_FILE" | sed '/^@/d' | sed '/^infixl /d' |\
    sed 's/^\[\(.*\)\]/\1/g' > defs.txt
