#!/usr/bin/env bash

set -e

FILENAME="$1"
BINARY="${FILENAME/.c/}"

cd "$(dirname "$0")"
uv run main.py "$@"

set +e

$BINARY
echo $?
