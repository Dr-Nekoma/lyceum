-module(lyceum_cluster_connector_SUITE).
-moduledoc """
The connector, exercised against real nodes.

Everything the connector does is about other VMs: dialing peers,
noticing they went away, and dialing them again. None of that can be
faked convincingly in a single node, so this suite starts two of them
(`peer` with a standard_io control channel, which keeps the test node
itself out of the topology).
""".

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-import(lyceum_cluster_test_helpers, [start_peer/1, stop_peer/1, wait_until/2]).

%% CT Callbacks
-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).

%% Test cases
-export([
    test_connects_to_peer/1,
    test_status/1,
    test_reconnects_after_peer_restart/1,
    test_node_type_visible_across_link/1
]).

%% Retrying on the production schedule would make the restart test wait
%% seconds for something that recovers in milliseconds.
-define(RETRY_BASE_MS, 100).

all() ->
    [
        test_connects_to_peer,
        test_status,
        test_reconnects_after_peer_restart,
        test_node_type_visible_across_link
    ].

init_per_suite(Config) ->
    case lyceum_cluster_test_helpers:peer_supported() of
        true -> Config;
        false -> {skip, "peer nodes are unavailable in this environment"}
    end.

end_per_suite(Config) ->
    Config.

init_per_testcase(_TestCase, Config) ->
    %% The service node comes up first: it is the one being dialed, and
    %% the logic node needs its name to configure `peers`.
    ServiceName = peer:random_name(lyceum_svc),
    {ServicePeer, ServiceNode} = start_service(ServiceName),
    {LogicPeer, LogicNode} =
        start_peer(#{
            name => peer:random_name(lyceum_logic),
            node_types => "logic",
            peers => [ServiceNode],
            env => [{lyceum_cluster, connect_retry_base_ms, ?RETRY_BASE_MS}]
        }),
    [
        {service_name, ServiceName},
        {service_peer, ServicePeer},
        {service_node, ServiceNode},
        {logic_peer, LogicPeer},
        {logic_node, LogicNode}
        | Config
    ].

end_per_testcase(_TestCase, Config) ->
    ok = stop_peer(?config(logic_peer, Config)),
    ok = stop_peer(?config(service_peer, Config)),
    Config.

%%--------------------------------------------------------------------
%% Test Cases
%%--------------------------------------------------------------------

test_connects_to_peer(Config) ->
    Logic = ?config(logic_peer, Config),
    Service = ?config(service_node, Config),
    %% Connecting is the connector's whole reason to exist: nothing in
    %% the service node knows the logic node is coming.
    ok = wait_until(fun() -> connected(Logic, Service) end, 100).

test_status(Config) ->
    Logic = ?config(logic_peer, Config),
    Service = ?config(service_node, Config),
    ok = wait_until(fun() -> connected(Logic, Service) end, 100),

    Status = peer:call(Logic, lyceum_cluster_connector, status, []),
    ?assertMatch(#{node_types := [logic], peers := [Service], missing := []}, Status),
    ?assert(lists:member(Service, maps:get(connected, Status))).

test_reconnects_after_peer_restart(Config) ->
    Logic = ?config(logic_peer, Config),
    Service = ?config(service_node, Config),
    ok = wait_until(fun() -> connected(Logic, Service) end, 100),

    %% A service node being restarted is routine (a deploy, a crash).
    %% The logic node has to find it again on its own.
    ok = stop_peer(?config(service_peer, Config)),
    ok = wait_until(fun() -> not connected(Logic, Service) end, 100),

    {Restarted, Service} = start_service(?config(service_name, Config)),
    ok = wait_until(fun() -> connected(Logic, Service) end, 200),
    ok = stop_peer(Restarted).

test_node_type_visible_across_link(Config) ->
    Logic = ?config(logic_peer, Config),
    Service = ?config(service_node, Config),
    ok = wait_until(fun() -> connected(Logic, Service) end, 100),

    %% `pg` syncs over the link, so each node can enumerate the other's
    %% layer. This is what nodes_of_type/1 is built on.
    ok = wait_until(
        fun() -> peer:call(Logic, lyceum_cluster, nodes_of_type, [service]) =:= [Service] end,
        100
    ),
    ?assertEqual([node_of(Logic)], peer:call(Logic, lyceum_cluster, nodes_of_type, [logic])),
    ?assertEqual([], peer:call(Logic, lyceum_cluster, nodes_of_type, [frontend])).

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

start_service(Name) ->
    start_peer(#{name => Name, node_types => "service"}).

connected(Peer, Node) ->
    lists:member(Node, peer:call(Peer, erlang, nodes, [])).

node_of(Peer) ->
    peer:call(Peer, erlang, node, []).
