#!/usr/bin/env bash

set -euo pipefail

PROJECT_NAME=${PROJECT_NAME:-lyceum}
DEPLOY_USER=${DEPLOY_USER:-deploy}
DEPLOY_HOST=""

while [[ "$#" -gt 0 ]]; do
    case "$1" in
        --deploy-host)
            DEPLOY_HOST="$2"
            shift 2
            ;;
        --deploy-user)
            DEPLOY_USER="$2"
            shift 2
            ;;
        *)
            echo "Unknown option: $1"
            exit 1
            ;;
    esac
done

if [[ -z "$DEPLOY_HOST" ]]; then
    echo "Usage: $0 --deploy-host <hostname> [--deploy-user <user>]?"
    exit 1
fi

# TODO: Copy the /ebin folder
echo "[SYNC] Pushing new version to server..."
rsync \
    -azP \
    --mkpath \
    --delete \
    --exclude-from .rsyncignore \
    . \
    $DEPLOY_USER@$DEPLOY_HOST:~/$PROJECT_NAME

echo "[DEPLOY] Running entrypoint script..."
PROJECT_NAME=$PROJECT_NAME \
ssh $DEPLOY_USER@$DEPLOY_HOST "cd ~/$PROJECT_NAME && ./deploy_entrypoint.sh"
