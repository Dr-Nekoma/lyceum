-module(layer_gating_SUITE).
-moduledoc """
Every supervisor in the umbrella decides its children from
`lyceum_cluster:hosts_layer/1`, so a single release can boot as a
frontend, a logic, a service or an all-in-one node.

These are pure `init/1` assertions: nothing is started, so the suite
says exactly which layer owns which process without needing a database
or a cluster.
""".

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%% CT Callbacks
-export([all/0, end_per_testcase/2]).

%% Test cases
-export([
    test_frontend_only/1,
    test_logic_only/1,
    test_service_only/1,
    test_all_in_one/1,
    test_service_worker_count_follows_env/1,
    test_unconfigured_node_refuses_to_boot/1
]).

all() ->
    [
        test_frontend_only,
        test_logic_only,
        test_service_only,
        test_all_in_one,
        test_service_worker_count_follows_env,
        test_unconfigured_node_refuses_to_boot
    ].

end_per_testcase(_TestCase, Config) ->
    ok = application:unset_env(lyceum_cluster, node_types),
    ok = application:unset_env(lyceum_service, workers),
    Config.

%%--------------------------------------------------------------------
%% Test Cases
%%--------------------------------------------------------------------

test_frontend_only(_Config) ->
    set_node_types([frontend]),
    %% The client's front door, and nothing else.
    ?assertEqual([client_proxy_sup, simple_auth], child_ids(auth_sup)),
    ?assertEqual([], child_ids(player_root_sup)),
    ?assertEqual([], child_ids(lyceum_service_sup)),
    ?assertEqual([], child_ids(database_sup)),
    ?assertEqual([], child_ids(cache_sup)),
    ?assertEqual([], child_ids(world_sup)).

test_logic_only(_Config) ->
    set_node_types([logic]),
    ?assertEqual([player_session, player_top_level_sup], child_ids(player_root_sup)),
    %% The world is in-memory state for the player FSMs; the migration
    %% runner it normally sits behind belongs to the service layer.
    ?assertEqual([world], child_ids(world_sup)),
    ?assertEqual([], child_ids(auth_sup)),
    ?assertEqual([], child_ids(lyceum_service_sup)),
    ?assertEqual([], child_ids(database_sup)),
    ?assertEqual([], child_ids(cache_sup)).

test_service_only(_Config) ->
    set_node_types([service]),
    ok = application:set_env(lyceum_service, workers, 3),
    ?assertEqual(
        [{lyceum_service_worker, 1}, {lyceum_service_worker, 2}, {lyceum_service_worker, 3}],
        child_ids(lyceum_service_sup)
    ),
    ?assertEqual([{pgo_pool, lyceum_pool}, {pgo_pool, auth_pool}], child_ids(database_sup)),
    ?assertEqual([session_reaper], child_ids(cache_sup)),
    ?assertEqual([world_migrations], child_ids(world_sup)),
    ?assertEqual([], child_ids(auth_sup)),
    ?assertEqual([], child_ids(player_root_sup)).

test_all_in_one(_Config) ->
    %% What `just server` runs: every layer in one VM.
    set_node_types([frontend, logic, service]),
    ?assertEqual([client_proxy_sup, simple_auth], child_ids(auth_sup)),
    ?assertEqual([player_session, player_top_level_sup], child_ids(player_root_sup)),
    ?assertEqual([session_reaper], child_ids(cache_sup)),
    ok = application:set_env(lyceum_service, workers, 2),
    ?assertEqual([{pgo_pool, lyceum_pool}, {pgo_pool, auth_pool}], child_ids(database_sup)),
    ?assertEqual(
        [{lyceum_service_worker, 1}, {lyceum_service_worker, 2}],
        child_ids(lyceum_service_sup)
    ),
    %% rest_for_one, so migrations must still come before the world.
    ?assertEqual([world_migrations, world], child_ids(world_sup)).

test_service_worker_count_follows_env(_Config) ->
    set_node_types([service]),
    ok = application:set_env(lyceum_service, workers, 1),
    ?assertEqual([{lyceum_service_worker, 1}], child_ids(lyceum_service_sup)).

test_unconfigured_node_refuses_to_boot(_Config) ->
    %% No node_types at all is a misconfiguration, not a node that hosts
    %% nothing: every gated supervisor must fail loudly rather than boot
    %% empty and look healthy while serving no traffic.
    ok = application:unset_env(lyceum_cluster, node_types),
    Supervisors = [
        auth_sup,
        player_root_sup,
        lyceum_service_sup,
        database_sup,
        cache_sup,
        world_sup
    ],
    _ = [
        ?assertError({missing_config, node_types}, child_ids(Sup))
     || Sup <- Supervisors
    ],
    ok.

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

set_node_types(Types) ->
    ok = application:set_env(lyceum_cluster, node_types, Types).

child_ids(Sup) ->
    {ok, {_Flags, Specs}} = Sup:init([]),
    [maps:get(id, Spec) || Spec <- Specs].
