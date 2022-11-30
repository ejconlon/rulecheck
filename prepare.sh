#!/bin/bash

# Download and extract all packages from stackage

set -eux

pushd auto
  if [ ! -d .venv ]; then
    make venv
  fi

  PACKAGEDIR=../../haskell-packages
  LISTINGFILE=./listing.json
  RULESFILE=./rules.json

  # rm -rf ${PACKAGEDIR}
  mkdir -p ${PACKAGEDIR}

  rm -f "$RULESFILE"

  # uncomment to regenerate listing
  # ./run.sh main find --listing ${LISTINGFILE}

  ./run.sh main download --listing ${LISTINGFILE} --scratch ${PACKAGEDIR}
  ./run.sh main extract --listing ${LISTINGFILE} --scratch ${PACKAGEDIR} --output ${RULESFILE}
popd
