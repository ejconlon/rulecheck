#!/bin/bash

set -eu

MOD="$1"
shift

cd "$(dirname "$0")"

exec .venv/bin/python3 -m auto.${MOD} $@
