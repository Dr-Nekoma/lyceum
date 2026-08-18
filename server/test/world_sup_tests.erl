-module(world_sup_tests).

-include_lib("eunit/include/eunit.hrl").

%% The world app spans two layers: migrations belong to `service' and
%% the world process to `logic'. These tests run as a node hosting
%% both; layer_gating_SUITE covers what happens on nodes hosting only
%% one of them.
world_sup_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun starts_a_named_supervisor/0,
        fun migrations_come_before_the_world/0
    ]}.

setup() ->
    ok = application:set_env(lyceum_cluster, node_types, [logic, service]),
    meck:new(world, [non_strict]),
    meck:new(world_migrations, [non_strict]),
    meck:expect(world, start_link, fun idle_worker/0),
    meck:expect(world_migrations, start_link, fun idle_worker/0).

cleanup(_) ->
    meck:unload(),
    application:unset_env(lyceum_cluster, node_types).

starts_a_named_supervisor() ->
    {ok, Pid} = world_sup:start_link(),
    ?assertEqual(Pid, whereis(world_sup)),
    ?assert(is_process_alive(Pid)),
    stop(Pid).

migrations_come_before_the_world() ->
    {ok, {Flags, Children}} = world_sup:init([]),
    ?assertEqual(#{strategy => rest_for_one, intensity => 12, period => 3600}, Flags),
    %% Order matters: rest_for_one only restarts the world on a failed
    %% migration run if the migrations come first.
    ?assertEqual([child_spec(world_migrations), child_spec(world)], Children).

child_spec(Module) ->
    #{
        id => Module,
        start => {Module, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [Module]
    }.

idle_worker() ->
    {ok,
        spawn_link(fun Idle() ->
            receive
                _ -> Idle()
            end
        end)}.

stop(Pid) ->
    Ref = monitor(process, Pid),
    unlink(Pid),
    exit(Pid, shutdown),
    receive
        {'DOWN', Ref, process, Pid, _} -> ok
    after 1000 -> error(supervisor_did_not_stop)
    end.
