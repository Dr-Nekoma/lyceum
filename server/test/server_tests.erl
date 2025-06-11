-module(server_tests).

-include_lib("eunit/include/eunit.hrl").

start_test() ->
    meck:new(server_sup),
    meck:expect(server_sup, start_link, fun() -> {ok, mocked_pid} end),

    {ok, Result} = server:start(normal, []),

    ?assertEqual(mocked_pid, Result),
    ?assert(meck:called(server_sup, start_link, [])),

    meck:unload(server_sup).
    

stop_test() ->
    ?assertEqual(ok, server:stop(any_value)).
