#!/usr/bin/env bash

set -euxo pipefail

PG_URL=${PG_URL:-"postgresql://admin:admin@127.0.0.1:5432/mmo"}

echo "Setting PG_URL=${PG_URL}"

find ./database/migrations -iname "*.sql" | sort | xargs printf -- '-f %s\n' | xargs psql $PG_URL -1 -v ON_ERROR_STOP=1;
