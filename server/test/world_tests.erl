-module(world_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("include/world_state.hrl").

start_link_test() -> 
    meck:new(database),
    meck:new(migraterl),
    meck:new(map_generator),
    
    meck:expect(database, connect, fun() -> {ok, fake_connection} end),
    
    meck:expect(migraterl, migrate, fun(_,_,_) -> {ok, []} end),
    
    meck:expect(map_generator, create_map, fun(_,_,_) -> ok end),
    
    {ok, WorldPid} = world:start_link(),
    
    ?assert(is_pid(WorldPid)),
    ?assert(is_process_alive(WorldPid)),
    ?assertEqual(WorldPid, global:whereis_name(world)),
    ?assert(meck:called(database, connect, [])),
    ?assert(meck:num_calls(migraterl, migrate, 3) >= 1),
    ?assert(meck:num_calls(map_generator, create_map, 3) >= 1),
    
    global:unregister_name(world),

    meck:unload(database),
    meck:unload(migraterl),
    meck:unload(map_generator).

stop_test() ->
    ?assertEqual(ok, server:stop(any_value)).

init_test() ->
    meck:new(database),
    meck:new(migraterl),
    meck:new(map_generator),

    meck:expect(database, connect, fun() -> {ok, fake_connection} end),
    meck:expect(migraterl, migrate, fun(_,_,_) -> {ok, []} end),
    meck:expect(map_generator, create_map, fun(_,_,_) -> ok end),

    {ok, State} = world:init([]),

    ?assertMatch(#world_state{}, State),
    ?assertEqual(fake_connection, State#world_state.connection),
    ?assert(meck:called(database, connect, [])),
    ?assert(meck:num_calls(migraterl, migrate, 3) >= 1),
    ?assert(meck:num_calls(map_generator, create_map, 3) >= 1),

    meck:unload(database),
    meck:unload(migraterl),
    meck:unload(map_generator).
