-module(server_sup_tests).

-include_lib("eunit/include/eunit.hrl").

start_link_test() ->
    meck:new(database),
    meck:new(migraterl),
    meck:new(map_generator),

    meck:expect(database, connect, fun() -> {ok, fake_connection} end),
    meck:expect(migraterl, migrate, fun(_, _, _) -> {ok, []} end),
    meck:expect(map_generator, create_map, fun(_, _, _) -> ok end),

    meck:new(world_sup, [non_strict]),
    meck:new(dispatcher_sup, [non_strict]),
    meck:new(player_sup, [non_strict]),

    meck:expect(world_sup, start_link, fun() -> {ok, self()} end),
    meck:expect(dispatcher_sup, start_link, fun() -> {ok, self()} end),
    meck:expect(player_sup, start_link, fun() -> {ok, self()} end),

    {ok, Pid} = server_sup:start_link(),
    ?assert(is_pid(Pid)),
    ?assert(is_process_alive(Pid)),
    ?assertEqual(Pid, whereis(server_sup)),

    meck:unload(world_sup),
    meck:unload(dispatcher_sup),
    meck:unload(player_sup),
    meck:unload(database),
    meck:unload(migraterl),
    meck:unload(map_generator).

init_test() ->
    {ok, {SupFlags, ChildSpecs}} = server_sup:init([]),

    ExpectedFlags =
        #{strategy => one_for_one,
          intensity => 12,
          period => 3600},
    ?assertEqual(ExpectedFlags, SupFlags),

    ExpectedSpecs =
        [#{id => dispatcher_sup,
           start => {dispatcher_sup, start_link, []},
           restart => permanent,
           shutdown => 5000,
           type => supervisor,
           modules => [dispatcher_sup]},
         #{id => player_sup,
           start => {player_sup, start_link, []},
           restart => permanent,
           shutdown => 5000,
           type => supervisor,
           modules => [player_sup]},
         #{id => world_sup,
           start => {world_sup, start_link, []},
           restart => permanent,
           shutdown => 5000,
           type => supervisor,
           modules => [world_sup]}],

    ActualSpecsSorted =
        lists:sort(fun(A, B) -> maps:get(id, A) =< maps:get(id, B) end, ChildSpecs),

    ExpectedSpecsSorted =
        lists:sort(fun(A, B) -> maps:get(id, A) =< maps:get(id, B) end, ExpectedSpecs),

    ?assertEqual(ExpectedSpecsSorted, ActualSpecsSorted).
