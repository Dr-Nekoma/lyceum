set export := true

# --------------
# Internal Use
#      Or
# Documentation
# --------------
# Application

database := justfile_directory() + "/database"
server := justfile_directory() + "/src"
client := justfile_directory() + "/client"
server_port := "8080"

# Utils

replace := if os() == "linux" { "sed -i" } else { "sed -i '' -e" }

# Lists all availiable targets
default:
    just --list

[doc('Formats source code. Options = { "client" [Default], "server", "justfile" }')]
format source='client':
    #!/usr/bin/env bash
    set -euo pipefail
    t=$(echo {{ source }} | cut -f2 -d=)
    echo "Selected Target: $t"
    if [[ $t == "client" ]]; then
        zig fmt .
    elif [[ $t == "justfile" ]]; then
        just --fmt --unstable
    elif [[ $t == "server" ]]; then
        rebar3 fmt
    else
        echo "No formating selected, skipping step..."
    fi

# ---------
# Database
# ---------

# Login into the local Database as `admin`
db:
    psql -U admin mmo

# Bootstraps the local nix-based postgres server
postgres:
    devenv up

# -------
# Client
# -------

client:
    cd client && zig build run

client-build:
    cd client && zig build

client-test:
    cd client && zig build test

client-build-ci:
    cd client && zig build -fsys=raylib

client-test-ci:
    cd client && zig build test -fsys=raylib

client-deps:
    cd client && nix run github:Cloudef/zig2nix#zon2nix -- build.zig.zon > zon-deps.nix

# --------
# Backend
# --------

build:
    rebar3 compile

# Fetches rebar3 dependencies, updates both the rebar and nix lockfiles
deps:
    rebar3 get-deps
    rebar3 nix lock

# Runs ther erlang server (inside the rebar shell)
server: build
    rebar3 shell

# Runs unit tests in the server
test:
    rebar3 do eunit, ct

# --------
# Releases
# --------

# Create a prod release of the server
release: deps
    rebar3 as prod release

# Create a prod release (for nix) of the server
release-nix:
    rebar3 as prod tar

# ----------
# Deployment
# ----------

# Builds the deployment docker image with Nix
build-docker:
    nix build .#dockerImage

# Updates Heroku's registry with the new image
_update-registry:
    docker load < ./result
    docker tag lyceum:latest registry.heroku.com/lyceum/web
    docker push registry.heroku.com/lyceum/web

update-registry: build-docker
    _update-registry

update-registry-ci:
    _update-registry

# Release app with the new docker image on Heroku
heroku-release: update-registry
    heroku container:release -a=lyceum web

heroku-release-ci:
    heroku container:release -a=lyceum web
