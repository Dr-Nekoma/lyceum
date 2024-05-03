build:
	rebar3 compile

test:
	rebar3 do eunit, ct
