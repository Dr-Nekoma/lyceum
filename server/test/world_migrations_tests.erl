-module(world_migrations_tests).

-include_lib("eunit/include/eunit.hrl").

migrations_run_in_order_test() ->
    meck:new(database),
    meck:new(migraterl),
    meck:new(map_generator),

    meck:expect(database, open_migrator_connection, fun() -> {ok, fake_connection} end),
    meck:expect(database, close_migrator_connection, fun(_) -> ok end),
    meck:expect(migraterl, migrate, fun(_, _, _) -> {ok, []} end),
    meck:expect(map_generator, create_map, fun(_, _) -> ok end),

    {ok, State, {continue, migrate}} = world_migrations:init([]),
    {noreply, _Done} = world_migrations:handle_continue(migrate, State),

    ?assert(meck:called(database, open_migrator_connection, [])),
    ?assert(meck:called(database, close_migrator_connection, '_')),
    % main -> repeatable -> init -> test
    ?assertEqual(4, meck:num_calls(migraterl, migrate, 3)),
    Dirs =
        [
            filename:basename(Path)
         || {_Pid, {migraterl, migrate, [_Conn, Path, _Opts]}, _Ret} <- meck:history(migraterl)
        ],
    ?assertEqual(["main", "repeatable", "init", "test"], Dirs),
    ?assertEqual(1, meck:num_calls(map_generator, create_map, 2)),

    meck:unload(map_generator),
    meck:unload(migraterl),
    meck:unload(database).

retries_when_database_is_down_test() ->
    meck:new(database),
    meck:expect(database, open_migrator_connection, fun() -> {error, econnrefused} end),

    {ok, State, {continue, migrate}} = world_migrations:init([]),
    % No crash on connection failure, a retry gets scheduled instead
    % and loops back into the connection attempt.
    {noreply, Retrying} = world_migrations:handle_continue(migrate, State),
    ?assertMatch(
        {noreply, Retrying, {continue, migrate}},
        world_migrations:handle_info(retry, Retrying)
    ),
    receive
        retry ->
            ok
    after 2000 ->
        erlang:error(no_retry_scheduled)
    end,

    meck:unload(database).
