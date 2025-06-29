#!/usr/bin/env bash

# https://stackoverflow.com/a/11114547/4614840
SCRIPT=$(realpath "$0")
SCRIPTPATH=$(dirname "$SCRIPT")

set -euxo pipefail

PG_HOST=${PGHOST:-localhost}
PG_USER=${PGUSER:-admin}
PG_PASSWORD=${PGPASSWORD:-admin}
PG_DATABASE=${PGDATABASE:-mmo}
PG_URL=${PG_URL:-"postgresql://$PG_USER:$PG_PASSWORD@$PG_HOST:5432/$PG_DATABASE"}

echo "Setting PG_URL=${PG_URL}"

psql "$PG_URL" -1 -v ON_ERROR_STOP=1 -f "$SCRIPTPATH/main.down.sql"
