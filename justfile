client:
    cd client && zig build src/client.zig --search-prefix $ERLANG_PATH --search-prefix $RAYLIB_PATH run -- \"$@\"

client-test:
    cd client && zig build --search-prefix ${ERLANG_PATH} --search-prefix ${RAYLIB_PATH} test -- \"$@\"

build:
	rebar3 compile

test:
	rebar3 do eunit, ct
