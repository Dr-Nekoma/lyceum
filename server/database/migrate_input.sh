#!/usr/bin/env bash

# https://stackoverflow.com/a/11114547/4614840
SCRIPT=$(realpath "$0")
SCRIPTPATH=$(dirname "$SCRIPT")

set -euxo pipefail

PG_URL=${PG_URL:-"postgresql://admin:admin@127.0.0.1:5432/mmo"}

echo "Setting PG_URL=${PG_URL}"

psql $PG_URL -1 -v ON_ERROR_STOP=1 -f $SCRIPTPATH/main.input.sql
