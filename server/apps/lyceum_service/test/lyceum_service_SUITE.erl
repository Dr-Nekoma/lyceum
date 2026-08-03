-module(lyceum_service_SUITE).
-moduledoc """
Tests for the service-layer facade and its worker pool.

The suites drive the modules directly rather than booting the whole
application: a `pg` scope is started per testcase (same pattern as
`lyceum_cluster_SUITE`) and workers are started with `start_link/0`.
Database access is meck'd out, SQL correctness is `registry_SUITE`'s
job; here only the facade's own behaviour is under test.

Routing, the single re-pick on a stale pid and the mapping of dropped
connections onto error values belong to `lyceum_cluster:call/3` and are
covered by `lyceum_cluster_SUITE`. What is left here is what this
facade decides for itself: which worker group it talks to, what its
errors are called, and how long it waits.
""".

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%% CT Callbacks
-export([all/0, init_per_testcase/2, end_per_testcase/2]).

%% Test cases
-export([
    test_worker_joins_service/1,
    test_routes_to_local_worker/1,
    test_no_service/1,
    test_timeout_maps_to_service_timeout/1,
    test_call_timeout_is_configurable/1,
    test_unknown_request/1
]).

all() ->
    [
        test_worker_joins_service,
        test_routes_to_local_worker,
        test_no_service,
        test_timeout_maps_to_service_timeout,
        test_call_timeout_is_configurable,
        test_unknown_request
    ].

init_per_testcase(_TestCase, Config) ->
    ok = application:set_env(lyceum_cluster, node_types, [service]),
    ok = application:set_env(lyceum_service, call_timeout, 5000),
    Scope = lyceum_cluster_test_helpers:start_scope(),
    [{scope, Scope} | Config].

end_per_testcase(_TestCase, Config) ->
    ok = lyceum_cluster_test_helpers:stop_scope(?config(scope, Config)),
    ok = application:unset_env(lyceum_cluster, node_types),
    ok = application:unset_env(lyceum_service, call_timeout),
    _ = meck:unload(),
    Config.

%%--------------------------------------------------------------------
%% Test Cases
%%--------------------------------------------------------------------

test_worker_joins_service(_Config) ->
    {ok, Worker} = lyceum_service_worker:start_link(),
    ok = lyceum_cluster_test_helpers:wait_until(
        fun() -> lists:member(Worker, lyceum_cluster:members(service)) end
    ),
    ok = gen_server:stop(Worker).

test_routes_to_local_worker(_Config) ->
    meck:new(registry, [passthrough]),
    meck:expect(registry, check_user, fun(_Request, auth_pool) ->
        {ok, {42, "someone@example.com"}}
    end),
    {ok, Worker} = start_worker(),
    Result = lyceum_service:check_user(#{username => "someone", password => "hunter2"}),
    ?assertEqual({ok, {42, "someone@example.com"}}, Result),
    ?assert(meck:validate(registry)),
    ok = gen_server:stop(Worker).

test_no_service(_Config) ->
    ?assertEqual(
        {error, no_service},
        lyceum_service:check_user(#{username => "someone", password => "hunter2"})
    ).

test_timeout_maps_to_service_timeout(_Config) ->
    ok = application:set_env(lyceum_service, call_timeout, 50),
    meck:new(registry, [passthrough]),
    meck:expect(registry, check_user, fun(_Request, auth_pool) ->
        timer:sleep(500),
        {ok, {42, "someone@example.com"}}
    end),
    {ok, Worker} = start_worker(),
    Result = lyceum_service:check_user(#{username => "someone", password => "hunter2"}),
    ?assertEqual({error, service_timeout}, Result),
    ok = gen_server:stop(Worker).

test_call_timeout_is_configurable(_Config) ->
    %% The facade owns only two decisions now, what its failures are
    %% called and how long it waits, so the `call_timeout` env has to
    %% actually reach lyceum_cluster:call/3.
    ok = application:set_env(lyceum_service, call_timeout, 1),
    meck:new(registry, [passthrough]),
    meck:expect(registry, check_user, fun(_Request, auth_pool) ->
        timer:sleep(200),
        {ok, {42, "someone@example.com"}}
    end),
    {ok, Worker} = start_worker(),
    ?assertEqual(
        {error, service_timeout},
        lyceum_service:check_user(#{username => "someone", password => "hunter2"})
    ),
    ok = gen_server:stop(Worker).

test_unknown_request(_Config) ->
    {ok, Worker} = start_worker(),
    ?assertEqual({error, unknown_request}, gen_server:call(Worker, bogus)),
    ok = gen_server:stop(Worker).

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

start_worker() ->
    {ok, Worker} = lyceum_service_worker:start_link(),
    ok = lyceum_cluster_test_helpers:wait_until(
        fun() -> lists:member(Worker, lyceum_cluster:members(service)) end
    ),
    {ok, Worker}.
