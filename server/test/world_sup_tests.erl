-module(world_sup_tests).

-include_lib("eunit/include/eunit.hrl").

start_link_test() ->
    meck:new(world, [non_strict]),
    meck:new(world_migrations, [non_strict]),
    meck:expect(world, start_link, fun() ->
        {ok,
            spawn(fun() ->
                receive
                    stop -> ok
                end
            end)}
    end),
    meck:expect(world_migrations, start_link, fun() ->
        {ok,
            spawn(fun() ->
                receive
                    stop -> ok
                end
            end)}
    end),

    {ok, Pid} = world_sup:start_link(),

    ?assert(is_pid(Pid)),
    ?assert(is_process_alive(Pid)),
    ?assertEqual(Pid, whereis(world_sup)),

    meck:unload(world_migrations),
    meck:unload(world).

init_test() ->
    {ok, {SupFlags, [Migrations, WorldWorker]}} = world_sup:init([]),

    ExpectedFlags =
        #{
            strategy => rest_for_one,
            intensity => 12,
            period => 3600
        },

    ?assertEqual(ExpectedFlags, SupFlags),

    % Migrations must come first, the world only makes sense on top
    % of a migrated schema.
    ?assertEqual(world_migrations, maps:get(id, Migrations)),
    ?assertEqual({world_migrations, start_link, []}, maps:get(start, Migrations)),
    ?assertEqual(permanent, maps:get(restart, Migrations)),
    ?assertEqual(5000, maps:get(shutdown, Migrations)),
    ?assertEqual(worker, maps:get(type, Migrations)),
    ?assertEqual([world_migrations], maps:get(modules, Migrations)),

    ?assertEqual(world, maps:get(id, WorldWorker)),
    ?assertEqual({world, start_link, []}, maps:get(start, WorldWorker)),
    ?assertEqual(permanent, maps:get(restart, WorldWorker)),
    ?assertEqual(5000, maps:get(shutdown, WorldWorker)),
    ?assertEqual(worker, maps:get(type, WorldWorker)),
    ?assertEqual([world], maps:get(modules, WorldWorker)).
