#!/bin/bash

# Download and extract all packages from stackage

set -eux

mkdir -p ../haskell-packages
PACKAGEDIR=$(cd ../haskell-packages && pwd)
mkdir -p ../haskell-rules
RULESDIR=$(cd ../haskell-rules && pwd)

pushd auto
  if [ ! -d .venv ]; then
    make venv
  fi

  LISTINGFILE=./listing.json
  RULESFILE=./rules.json

  # rm -rf ${PACKAGEDIR}
  # mkdir -p ${PACKAGEDIR}

  rm -f "$RULESFILE"

  # uncomment to regenerate listing
  # ./run.sh main find --listing ${LISTINGFILE}

  ./run.sh main download --listing ${LISTINGFILE} --scratch ${PACKAGEDIR}
  ./run.sh main extract --listing ${LISTINGFILE} --scratch ${PACKAGEDIR} --output ${RULESFILE}
  ./run.sh main repack --listing ${LISTINGFILE} --output ${RULESDIR}
popd
