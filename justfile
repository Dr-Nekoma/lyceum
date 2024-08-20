client:
    cd client && zig build --search-prefix ${ERLANG_INTERFACE_PATH} --search-prefix ${RAYLIB_PATH} run -- \"$@\"

client-build:
    cd client && zig build --search-prefix ${ERLANG_INTERFACE_PATH} --search-prefix ${RAYLIB_PATH} -- \"$@\"

client-test:
    cd client && zig build --search-prefix ${ERLANG_INTERFACE_PATH} --search-prefix ${RAYLIB_PATH} test -- \"$@\"

build:
	rebar3 compile

server:
	rebar3 shell

test:
	rebar3 do eunit, ct

postgres:
	devenv up

format:
	zig fmt $(find ./ -type f \( -iname \*.zig \))
