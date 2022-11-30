#!/usr/bin/env sh

set -euo pipefail

HASKELL_PACKAGE_DIR=../haskell-packages
VENDORED_DIR=./vendored
find "$VENDORED_DIR" -type f  -print0 |
while IFS= read -r -d '' SRCFILE; do
    # HACK
    BASENAME=$(echo "$SRCFILE" | cut -c 12-)
    DESTFILE="$HASKELL_PACKAGE_DIR/$BASENAME"
    cp "$SRCFILE" "$DESTFILE"
done
