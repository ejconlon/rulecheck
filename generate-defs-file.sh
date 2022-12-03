#!/usr/bin/env sh

set -euo pipefail

HOOGLE_FILE=$(cabal haddock --haddock-hoogle | tail -n1)

# Bash comments cannot be placed inline, so here's what happens
# The command sed 's/^\[\(.*\)\]/\1/g' removes replaces [ID] with ID for haddock hidden syntax
# See: https://haskell-haddock.readthedocs.io/en/latest/invoking.html#cmdoption-hoogle
# The final command replaces [x] with List x
sed '/^--/d' "$HOOGLE_FILE" | sed '/^@/d' | sed '/^infixl /d' |\
    sed 's/^\[\(.*\)\]/\1/g' > defs.txt
