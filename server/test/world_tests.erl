-module(world_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("include/world_state.hrl").

% world:init/1 no longer touches the database, migrations belong to
% world_migrations. Its init must stay pure and fast.
init_test() ->
    {ok, State} = world:init([]),
    ?assertMatch(#world_state{}, State).

start_link_test() ->
    {ok, WorldPid} = world:start_link(),

    ?assert(is_pid(WorldPid)),
    ?assert(is_process_alive(WorldPid)),
    ?assertEqual(WorldPid, whereis(world)),

    gen_server:stop(WorldPid).
