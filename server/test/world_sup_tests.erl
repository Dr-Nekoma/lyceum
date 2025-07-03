-module(world_sup_tests).

-include_lib("eunit/include/eunit.hrl").

start_link_test() ->
    meck:new(world, [non_strict]),
    meck:expect(world, start_link, fun() -> {ok, self()} end),

    {ok, Pid} = world_sup:start_link(),

    ?assert(is_pid(Pid)),
    ?assert(is_process_alive(Pid)),
    ?assertEqual(Pid, whereis(world_sup)),

    meck:unload(world).

init_test() ->
    {ok, {SupFlags, [WorldWorker]}} = world_sup:init([]),

    ExpectedFlags =
        #{strategy => one_for_one,
          intensity => 12,
          period => 3600},

    ?assertEqual(ExpectedFlags, SupFlags),
    ?assertEqual(world, maps:get(id, WorldWorker)),
    ?assertEqual({world, start_link, []}, maps:get(start, WorldWorker)),
    ?assertEqual(permanent, maps:get(restart, WorldWorker)),
    ?assertEqual(brutal_kill, maps:get(shutdown, WorldWorker)),
    ?assertEqual(worker, maps:get(type, WorldWorker)),
    ?assertEqual([world], maps:get(modules, WorldWorker)).
