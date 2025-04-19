set export := true
set dotenv-load := true

# --------------
# Internal Use
#      Or
# Documentation
# --------------
# Application

database := justfile_directory() + "server/database"
server := justfile_directory() + "/server"
client := justfile_directory() + "/client"
server_port := "8080"

# Deploy
deploy_host := env_var_or_default("DEPLOY_HOST", "NONE")

# Utils

replace := if os() == "linux" { "sed -i" } else { "sed -i '' -e" }

# Aliases
alias d := dialyzer
alias dbu := db-up
alias dbd := db-down
alias dbr := db-reset
alias dbi := db-input

# Lists all availiable targets
default:
    just --list

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
format:
    zig fmt .

client:
    cd client && zig build run

client-release:
    cd client && zig build run --release=fast

client-build:
    cd client && zig build

client-watch:
    cd client && zig build -Dno-bin --watch -fincremental

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
    cd server && rebar3 compile

# Fetches rebar3 dependencies, updates both the rebar and nix lockfiles
deps:
    cd server && rebar3 get-deps
    cd server && rebar3 nix lock

# Runs dializer on the erlang codebase
dialyzer:
    cd server && rebar3 as dialzye dialyzer

# Spawns an erlang shell
shell: build
    cd server && rebar3 shell

# Runs ther erlang server (inside the rebar shell)
server: build
    #!/usr/bin/env bash
    cd server && \
        rebar3 release -n server && \
        ./_build/default/rel/server/bin/server foreground

# Runs unit tests in the server
test:
    cd server && rebar3 do eunit, ct

# Migrates the DB (up)
db-up:
    ./server/database/migrate_up.sh

# Nukes the DB
db-down:
    ./server/database/migrate_down.sh

# Populate DB
db-input:
    ./server/database/migrate_input.sh

# Hard reset DB
db-reset: db-down db-up db-input

# --------
# Releases
# --------

# Create a prod release of all apps
release:
    rebar3 as prod release -n server

# Create a prod release (for nix) of the server
release-nix:
    rebar3 as prod tar

# ----------
# Deployment
# ----------

# Builds the deployment docker image with Nix
build-docker:
    nix build .#dockerImage

# Builds and deploys a release in the host VM
deploy:
    @echo "Attemping to deploy to: {{deploy_host}}"
    ./deploy.sh --deploy-host {{deploy_host}}

# Starts the deployed code
start:
    #!/usr/bin/env bash
    export SERVER_APP=lyceum_server
    export ERL_DIST_PORT=8080
    if [[ $(./result/bin/$SERVER_APP ping) == "pong" ]]; then
        ./result/bin/$SERVER_APP stop
        ./result/bin/$SERVER_APP foreground
    else
        ./result/bin/$SERVER_APP foreground
    fi

