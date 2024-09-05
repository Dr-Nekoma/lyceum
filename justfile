client:
    cd client && zig build --search-prefix ${ERLANG_INTERFACE_PATH} --search-prefix ${RAYLIB_PATH} run -- \"$@\"

client-build:
    cd client && zig build --search-prefix ${ERLANG_INTERFACE_PATH} --search-prefix ${RAYLIB_PATH} -- \"$@\"

client-test:
    cd client && zig build --search-prefix ${ERLANG_INTERFACE_PATH} --search-prefix ${RAYLIB_PATH} test -- \"$@\"

build:
	rebar3 compile

# Fetches rebar3 dependencies, updates both the rebar and nix lockfiles
deps:
	rebar3 get-deps
	rebar3 nix lock

server:
	rebar3 shell

test:
	rebar3 do eunit, ct

postgres:
	devenv up

format:
	zig fmt $(find ./ -type f \( -iname \*.zig \))

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
update-registry: build-docker
    docker load < ./result
    docker tag lyceum:latest registry.heroku.com/lyceum/web
    docker push registry.heroku.com/lyceum/web

# Release app with the new docker image on Heroku
heroku-release: update-registry
    heroku container:release -a=lyceum web

