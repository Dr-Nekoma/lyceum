-module(lyceum_cluster_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-import(lyceum_cluster_test_helpers,
        [start_scope/0, stop_scope/1, spawn_worker/1, spawn_responder/2, stop_worker/1,
         wait_until/1]).

%% CT Callbacks
-export([all/0, groups/0, init_per_group/2, end_per_group/2, init_per_testcase/2,
         end_per_testcase/2]).
%% Test cases
-export([test_node_types_sorted/1, test_node_types_missing/1, test_node_types_invalid/1,
         test_hosts_layer/1, test_node_types_from_string/1, test_node_types_bad_string/1,
         test_peers_from_string/1, test_peers_bad_string/1]).
-export([test_pick_empty_group/1, test_join_and_members/1, test_pick_prefers_local/1,
         test_membership_dropped_on_death/1, test_nodes_of_type/1]).
-export([test_call_routes_to_a_member/1, test_call_unavailable/1, test_call_timeout/1,
         test_call_repicks_once_after_noproc/1, test_call_default_errors/1]).

%%--------------------------------------------------------------------
%% CT Callbacks
%%--------------------------------------------------------------------

all() ->
    [{group, configuration}, {group, discovery}, {group, calling}].

groups() ->
    [{configuration,
      [test_node_types_sorted, test_node_types_missing, test_node_types_invalid,
       test_hosts_layer, test_node_types_from_string, test_node_types_bad_string,
       test_peers_from_string, test_peers_bad_string]},
     {discovery,
      [test_pick_empty_group, test_join_and_members, test_pick_prefers_local,
       test_membership_dropped_on_death, test_nodes_of_type]},
     {calling,
      [test_call_routes_to_a_member, test_call_unavailable, test_call_timeout,
       test_call_repicks_once_after_noproc, test_call_default_errors]}].

init_per_group(Name, Config) ->
    ct:pal("Starting group: ~p~n", [Name]),
    Config.

end_per_group(Name, Config) ->
    ct:pal("Ending Group: ~p~n", [Name]),
    Config.

init_per_testcase(TestCase, Config) ->
    %% The suite drives the API directly rather than booting the whole
    %% application, so the pg scope is started on its own. peers stays
    %% empty, no test here needs a second node.
    ok = application:set_env(lyceum_cluster, node_types, [frontend, logic, service]),
    ok = application:set_env(lyceum_cluster, peers, []),
    Pid = start_scope(),
    ct:pal("[~p] pg scope started at ~p", [TestCase, Pid]),
    [{scope_pid, Pid} | Config].

end_per_testcase(TestCase, Config) ->
    ct:comment("Ending test case: ~p", [TestCase]),
    _ = meck:unload(),
    stop_scope(?config(scope_pid, Config)),
    ok = application:unset_env(lyceum_cluster, node_types),
    ok = application:unset_env(lyceum_cluster, peers),
    Config.

%%--------------------------------------------------------------------
%% Test Cases
%%--------------------------------------------------------------------
%% Configuration
test_node_types_sorted(_Config) ->
    %% Duplicates and ordering in the config must not leak into the
    %% result, callers compare these lists.
    ok = application:set_env(lyceum_cluster, node_types, [service, frontend, service]),
    ?assertEqual([frontend, service], lyceum_cluster:node_types()).

test_node_types_missing(_Config) ->
    %% A node with no declared layers must refuse to run rather than
    %% guess which traffic it should serve.
    ok = application:unset_env(lyceum_cluster, node_types),
    ?assertError({missing_config, node_types}, lyceum_cluster:node_types()).

test_node_types_invalid(_Config) ->
    ok = application:set_env(lyceum_cluster, node_types, [frontend, database]),
    ?assertError({invalid_config, node_types, _}, lyceum_cluster:node_types()),

    ok = application:set_env(lyceum_cluster, node_types, []),
    ?assertError({invalid_config, node_types, _}, lyceum_cluster:node_types()),

    ok = application:set_env(lyceum_cluster, node_types, frontend),
    ?assertError({invalid_config, node_types, _}, lyceum_cluster:node_types()).

test_hosts_layer(_Config) ->
    ok = application:set_env(lyceum_cluster, node_types, [frontend]),
    ?assert(lyceum_cluster:hosts_layer(frontend)),
    ?assertNot(lyceum_cluster:hosts_layer(logic)),
    ?assertNot(lyceum_cluster:hosts_layer(service)).

test_node_types_from_string(_Config) ->
    %% What sys.config.src produces: $LYCEUM_NODE_TYPES expanded into
    %% the config file as a bare string.
    ok = application:set_env(lyceum_cluster, node_types, "frontend,logic"),
    ?assertEqual([frontend, logic], lyceum_cluster:node_types()),

    %% Whitespace around the separators is a typo, not a different
    %% configuration.
    ok = application:set_env(lyceum_cluster, node_types, " service , frontend "),
    ?assertEqual([frontend, service], lyceum_cluster:node_types()),

    ok = application:set_env(lyceum_cluster, node_types, "service"),
    ?assertEqual([service], lyceum_cluster:node_types()),

    ok = application:set_env(lyceum_cluster, node_types, <<"logic,service">>),
    ?assertEqual([logic, service], lyceum_cluster:node_types()).

test_node_types_bad_string(_Config) ->
    %% An unset LYCEUM_NODE_TYPES expands to the empty string, which is
    %% as much of a non-answer as leaving the key out entirely.
    ok = application:set_env(lyceum_cluster, node_types, ""),
    ?assertError({invalid_config, node_types, _}, lyceum_cluster:node_types()),

    ok = application:set_env(lyceum_cluster, node_types, "frontend,database"),
    ?assertError({invalid_config, node_types, _}, lyceum_cluster:node_types()),

    %% A typo must fail without minting an atom for itself.
    ok = application:set_env(lyceum_cluster, node_types, "frontned"),
    ?assertError({invalid_config, node_types, _}, lyceum_cluster:node_types()),
    ?assertError(badarg, list_to_existing_atom("frontned")).

test_peers_from_string(_Config) ->
    ok = application:unset_env(lyceum_cluster, peers),
    ?assertEqual([], lyceum_cluster:peers()),

    %% An all-in-one node has nothing below it to dial.
    ok = application:set_env(lyceum_cluster, peers, ""),
    ?assertEqual([], lyceum_cluster:peers()),

    ok = application:set_env(lyceum_cluster, peers, "lyceum_logic@host, lyceum_svc@host"),
    ?assertEqual(['lyceum_logic@host', 'lyceum_svc@host'], lyceum_cluster:peers()),

    %% The Erlang term form keeps working, that is what shell.config and
    %% the test suites use.
    ok = application:set_env(lyceum_cluster, peers, ['lyceum_logic@host']),
    ?assertEqual(['lyceum_logic@host'], lyceum_cluster:peers()).

test_peers_bad_string(_Config) ->
    %% A name without a host is not a node, and would silently never
    %% connect if it were accepted here.
    ok = application:set_env(lyceum_cluster, peers, "lyceum_logic"),
    ?assertError({invalid_config, peers, _}, lyceum_cluster:peers()),

    ok = application:set_env(lyceum_cluster, peers, "@host"),
    ?assertError({invalid_config, peers, _}, lyceum_cluster:peers()),

    ok = application:set_env(lyceum_cluster, peers, #{node => 'lyceum_logic@host'}),
    ?assertError({invalid_config, peers, _}, lyceum_cluster:peers()).

%% Discovery
test_pick_empty_group(_Config) ->
    %% The contract every *_api module is built on: an absent layer is
    %% a value to handle, never an exception.
    ?assertEqual({error, no_service}, lyceum_cluster:pick(service)),
    ?assertEqual([], lyceum_cluster:members(service)).

test_join_and_members(_Config) ->
    Self = self(),
    ok = lyceum_cluster:join(service),
    ?assertEqual([Self], lyceum_cluster:members(service)),
    ?assertEqual(Self, lyceum_cluster:pick(service)),

    ok = lyceum_cluster:leave(service),
    ?assertEqual([], lyceum_cluster:members(service)),
    ?assertEqual({error, no_service}, lyceum_cluster:pick(service)).

test_pick_prefers_local(_Config) ->
    %% Everything is local in a single-node suite, so this asserts the
    %% weaker but still useful property: every member is visible both
    %% globally and locally, which is what makes the local-first branch
    %% of pick/1 the one that runs.
    Workers = [spawn_worker(service) || _ <- lists:seq(1, 5)],
    ?assertEqual(lists:sort(Workers), lists:sort(lyceum_cluster:members(service))),
    ?assertEqual(lists:sort(Workers), lists:sort(lyceum_cluster:local_members(service))),

    _ = [stop_worker(W) || W <- Workers],
    ok.

test_membership_dropped_on_death(_Config) ->
    %% pg monitors members, so a crashed worker must not linger in the
    %% group. Without this, pick/1 would keep handing out dead pids.
    Worker = spawn_worker(service),
    ?assertEqual([Worker], lyceum_cluster:members(service)),

    stop_worker(Worker),
    ok = wait_until(fun() -> lyceum_cluster:members(service) =:= [] end),
    ?assertEqual({error, no_service}, lyceum_cluster:pick(service)).

test_nodes_of_type(_Config) ->
    ?assertEqual([], lyceum_cluster:nodes_of_type(logic)),

    ok = lyceum_cluster:join({node_type, logic}),
    ?assertEqual([node()], lyceum_cluster:nodes_of_type(logic)),
    ?assertEqual([], lyceum_cluster:nodes_of_type(frontend)).

%% Calling
test_call_routes_to_a_member(_Config) ->
    Responder = spawn_responder(service, fun({greet, Name}) -> {hello, Name} end),
    %% The callee's reply is passed through untouched.
    ?assertEqual({hello, "world"}, lyceum_cluster:call(service, {greet, "world"}, opts())),
    stop_worker(Responder).

test_call_unavailable(_Config) ->
    %% An empty group is an ordinary condition, not an exception: this
    %% is what a client sees as "that layer is down".
    ?assertEqual({error, no_logic}, lyceum_cluster:call(logic, ping, opts())).

test_call_timeout(_Config) ->
    Responder = spawn_responder(service, fun(ping) ->
                                            timer:sleep(500),
                                            pong
                                         end),
    ?assertEqual(
        {error, took_too_long},
        lyceum_cluster:call(service, ping, maps:put(timeout, 50, opts()))
    ),
    stop_worker(Responder).

test_call_repicks_once_after_noproc(_Config) ->
    %% pg is eventually consistent, so a pid can already be dead when
    %% it is handed out. Rather than race a real removal, the group
    %% lookup is made to answer with a dead pid exactly once.
    Responder = spawn_responder(service, fun(ping) -> pong end),
    Dead = dead_pid(),
    Lookups = ets:new(lookups, [public]),
    true = ets:insert(Lookups, {count, 0}),
    ok = meck:new(pg, [passthrough, unstick]),
    ok = meck:expect(pg, get_local_members, fun(Scope, Group) ->
                                               case ets:update_counter(Lookups, count, 1) of
                                                   1 -> [Dead];
                                                   _ -> meck:passthrough([Scope, Group])
                                               end
                                            end),

    ?assertEqual(pong, lyceum_cluster:call(service, ping, opts())),
    %% Exactly one retry: a second noproc means the whole view is
    %% stale and retrying again would only add latency.
    ?assertEqual([{count, 2}], ets:lookup(Lookups, count)),

    ets:delete(Lookups),
    stop_worker(Responder).

test_call_default_errors(_Config) ->
    %% Callers that do not care about naming still get a value back.
    ?assertEqual({error, no_service}, lyceum_cluster:call(service, ping)),

    Responder = spawn_responder(service, fun(ping) ->
                                            timer:sleep(500),
                                            pong
                                         end),
    ?assertEqual({error, timeout}, lyceum_cluster:call(service, ping, #{timeout => 50})),
    stop_worker(Responder).

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

%% Deliberately not the defaults, so a passing test proves the caller's
%% own error names came through rather than the built-in ones.
opts() ->
    #{unavailable => no_logic, timed_out => took_too_long}.

dead_pid() ->
    Pid = spawn(fun() -> ok end),
    ok = lyceum_cluster_test_helpers:wait_until(fun() -> not is_process_alive(Pid) end),
    Pid.
