#!/usr/bin/env bash

set -e

FILENAME="$1"
BINARY="${FILENAME/.c/}"

cd "$(dirname "$0")"
uv run main.py "$FILENAME"

set +e

$BINARY
echo $?
