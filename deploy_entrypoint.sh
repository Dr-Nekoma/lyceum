#!/usr/bin/env bash

export ERL_DIST_PORT=5000

# AAAAAAAAAAAAA, forgot that I have to build this again...
nix build .#server

if [[ $(./result/bin/server ping) == "pong" ]]; then
    echo "Restarting app..."
    ./result/bin/server restart -detached
else
    echo "Starting app..."
    ./result/bin/server foreground -detached
fi

