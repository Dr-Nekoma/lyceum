-module(cluster_SUITE).
-moduledoc """
The three-layer topology, running as three real nodes.

Everything else in the suite tree runs in one VM, where "the frontend
cannot reach the service layer" is a claim about configuration. Here it
is a claim about the network, and this is the only place it can be
tested: the nodes are started with `connect_all false` and linked
frontend -> logic -> service, so the frontend never opens a connection
to the service node, and `pg` (which does not relay across an
intermediate node) cannot show it the service groups.

The test node itself talks to the peers over their standard_io control
channel rather than over distribution, so it is not part of the
topology it is measuring.
""".

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-import(lyceum_cluster_test_helpers, [start_peer/1, stop_peer/1, wait_until/2]).

%% CT Callbacks
-export([all/0, init_per_suite/1, end_per_suite/1]).

%% Test cases
-export([
    test_topology/1,
    test_layer_isolation/1,
    test_login_e2e/1,
    test_two_service_nodes_share_sessions/1,
    test_service_reconnect/1
]).

%% Runs on the frontend node
-export([client_session/2]).
%% Runs on a service node
-export([open_session/3]).

-include("player_state.hrl").

-define(RETRY_BASE_MS, 100).
-define(CLIENT_TIMEOUT, 5000).

all() ->
    [
        test_topology,
        test_layer_isolation,
        test_login_e2e,
        test_two_service_nodes_share_sessions,
        %% Last: it takes the service node down and back up, so anything
        %% needing a settled cluster has already run.
        test_service_reconnect
    ].

init_per_suite(Config) ->
    case lyceum_cluster_test_helpers:peer_supported() of
        false ->
            {skip, "peer nodes are unavailable in this environment"};
        true ->
            %% Bottom-up, since peers point downward: a node needs the
            %% name of the layer below it before it can be configured.
            ServiceName = peer:random_name(lyceum_svc),
            {ServicePeer, ServiceNode} = start_service(ServiceName),
            {LogicPeer, LogicNode} =
                start_peer(#{
                    name => peer:random_name(lyceum_logic),
                    node_types => "logic",
                    peers => [ServiceNode],
                    apps => [player, world],
                    env => [{lyceum_cluster, connect_retry_base_ms, ?RETRY_BASE_MS}]
                }),
            {FrontendPeer, FrontendNode} =
                start_peer(#{
                    name => peer:random_name(lyceum_frontend),
                    node_types => "frontend",
                    peers => [LogicNode],
                    apps => [auth],
                    env => [{lyceum_cluster, connect_retry_base_ms, ?RETRY_BASE_MS}]
                }),
            ok = wait_until(fun() -> connected(FrontendPeer, LogicNode) end, 200),
            ok = wait_until(fun() -> connected(LogicPeer, ServiceNode) end, 200),
            [
                {service_name, ServiceName},
                {service_peer, ServicePeer},
                {service_node, ServiceNode},
                {logic_peer, LogicPeer},
                {logic_node, LogicNode},
                {frontend_peer, FrontendPeer},
                {frontend_node, FrontendNode}
                | Config
            ]
    end.

end_per_suite(Config) ->
    _ = [stop_peer(?config(Key, Config)) || Key <- [frontend_peer, logic_peer, service_peer]],
    Config.

%%--------------------------------------------------------------------
%% Test Cases
%%--------------------------------------------------------------------

test_topology(Config) ->
    %% Each node sees only its neighbours: no transitive connection is
    %% ever made, which is what `connect_all false` buys.
    ?assertEqual([?config(logic_node, Config)], nodes_of(?config(frontend_peer, Config))),
    ?assertEqual([?config(logic_node, Config)], nodes_of(?config(service_peer, Config))),
    ?assertEqual(
        lists:sort([?config(frontend_node, Config), ?config(service_node, Config)]),
        lists:sort(nodes_of(?config(logic_peer, Config)))
    ).

test_layer_isolation(Config) ->
    Frontend = ?config(frontend_peer, Config),
    Logic = ?config(logic_peer, Config),
    Service = ?config(service_peer, Config),

    %% The frontend has no route to the service layer, by construction.
    %% This is the invariant the whole login rework rests on: auth data
    %% has to travel through the logic layer.
    ?assertEqual({error, no_service}, pick(Frontend, service)),
    ?assertEqual([], nodes_of_type(Frontend, service)),

    %% ... while the logic layer, which is allowed to reach it, does.
    ok = wait_until(fun() -> is_pid(pick(Logic, service)) end, 200),
    ?assertEqual([?config(service_node, Config)], nodes_of_type(Logic, service)),

    %% The frontend can reach the session manager one layer down.
    ok = wait_until(fun() -> is_pid(pick(Frontend, logic)) end, 200),

    %% Isolation is symmetric: the service layer cannot see clients
    %% either, so nothing there can accidentally address a proxy.
    ?assertEqual([], nodes_of_type(Service, frontend)).

test_login_e2e(Config) ->
    Service = ?config(service_peer, Config),
    Frontend = ?config(frontend_peer, Config),
    case database_available(Service) of
        false ->
            {skip, "PostgreSQL is not reachable, skipping the end-to-end login"};
        true ->
            {Username, Email, Password} = create_user(Service),

            %% The whole session runs in one process on the frontend
            %% node, exactly like the Zig client's C-node: the proxy
            %% replies to the process that logged in, and everything
            %% below is reached through the handler it hands back.
            Session =
                peer:call(Frontend, ?MODULE, client_session, [Username, Password], ?CLIENT_TIMEOUT),
            #{handler := Handler, email := ReturnedEmail, characters := Characters} = Session,

            ?assertEqual(Email, ReturnedEmail),

            %% A pid on the frontend node. Handing the client anything
            %% else would be unreachable for it: its single distribution
            %% connection goes to the frontend and dist does not relay.
            ?assertEqual(?config(frontend_node, Config), node(Handler)),

            %% A round trip through frontend -> logic -> service and
            %% back: an answer of any shape means the SQL ran on the
            %% service node and the reply found its way home.
            ?assertEqual({ok, []}, Characters)
    end.

test_two_service_nodes_share_sessions(Config) ->
    Service1 = ?config(service_peer, Config),
    Logic = ?config(logic_peer, Config),
    LogicNode = ?config(logic_node, Config),
    case database_available(Service1) of
        false ->
            {skip, "PostgreSQL is not reachable, skipping the second service node"};
        true ->
            {Service2, Service2Node} = start_service(peer:random_name(lyceum_svc_b)),
            try
                %% Downward, like every other link here: the logic layer
                %% reaches a second service node the same way it reached
                %% the first.
                true = peer:call(Logic, net_kernel, connect_node, [Service2Node]),
                ok = wait_until(
                    fun() -> length(nodes_of_type(Logic, service)) =:= 2 end, 200
                ),

                %% The point of the whole exercise. With sessions in
                %% mnesia these two nodes each had their own private
                %% table and would both have said "nobody was logged
                %% in"; the second login would not have kicked the
                %% first, and the player would have had two live
                %% sessions. Sharing one Postgres row is what makes the
                %% second node see the first node's session.
                PlayerId = unique_player_id(),
                {ok, First, undefined} =
                    peer:call(Service1, ?MODULE, open_session, [PlayerId, "a", LogicNode]),
                {ok, _Second, Previous} =
                    peer:call(Service2, ?MODULE, open_session, [PlayerId, "b", LogicNode]),

                ?assertEqual(First, Previous),
                purge_session(Service1, PlayerId)
            after
                stop_peer(Service2)
            end
    end.

test_service_reconnect(Config) ->
    Logic = ?config(logic_peer, Config),
    Service = ?config(service_node, Config),
    ok = wait_until(fun() -> is_pid(pick(Logic, service)) end, 200),

    %% A service node restarting (deploy, crash, host reboot) must not
    %% need anything restarted above it.
    ok = stop_peer(?config(service_peer, Config)),
    ok = wait_until(fun() -> pick(Logic, service) =:= {error, no_service} end, 200),

    {Restarted, Service} = start_service(?config(service_name, Config)),
    ok = wait_until(fun() -> is_pid(pick(Logic, service)) end, 400),
    ok = stop_peer(Restarted).

%%--------------------------------------------------------------------
%% Client side, evaluated on the frontend node
%%--------------------------------------------------------------------
-doc """
One full client session, in one process, speaking the same protocol
the Zig client speaks: `{self(), {login, _}}` to the registered
`lyceum_server`, then requests straight to the handler that comes back,
then `logout`.

It has to be a single process. The proxy replies to whoever logged in,
so a session split across processes would leave the answers going to a
process that is no longer listening.
""".
-spec client_session(string(), string()) -> map() | {error, term()}.
client_session(Username, Password) ->
    lyceum_server ! {self(), {login, #{username => Username, password => Password}}},
    receive
        {ok, {Handler, Email}} ->
            Handler ! {list_characters, #{username => Username, email => Email}},
            Characters = await_reply(),
            Handler ! logout,
            #{handler => Handler, email => Email, characters => Characters};
        {error, Reason} ->
            {error, Reason}
    after 4000 -> {error, login_timeout}
    end.

await_reply() ->
    receive
        Reply -> Reply
    after 4000 -> {error, reply_timeout}
    end.

%%--------------------------------------------------------------------
%% Service side, evaluated on a service node
%%--------------------------------------------------------------------
-doc """
Opens a session for `PlayerId` from whichever service node runs this.

The client pid is created here so that it belongs to this node, and
returned so the caller can compare it with what the *other* service
node reports as the previous session.
""".
-spec open_session(integer(), string(), node()) ->
    {ok, pid(), pid() | undefined} | {error, term()}.
open_session(PlayerId, Suffix, OwnerNode) ->
    Client = spawn(fun() ->
        receive
            stop -> ok
        end
    end),
    Cache = #player_cache{
        player_id = PlayerId,
        username = "svc_" ++ Suffix,
        email = "svc_" ++ Suffix ++ "@example.com",
        client_pid = Client
    },
    case cache:login(Cache, OwnerNode) of
        {ok, _Stored, Previous} -> {ok, Client, Previous};
        {error, _} = Error -> Error
    end.

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

start_service(Name) ->
    start_peer(#{
        name => Name,
        node_types => "service",
        apps => [lyceum_service],
        env => [{lyceum_cluster, connect_retry_base_ms, ?RETRY_BASE_MS}]
    }).

nodes_of(Peer) ->
    peer:call(Peer, erlang, nodes, []).

connected(Peer, Node) ->
    lists:member(Node, nodes_of(Peer)).

pick(Peer, Group) ->
    peer:call(Peer, lyceum_cluster, pick, [Group]).

nodes_of_type(Peer, Type) ->
    peer:call(Peer, lyceum_cluster, nodes_of_type, [Type]).

-doc """
Whether the end-to-end login can run at all.

Reachability is not enough: `SELECT 1` succeeds against a database that
has never been migrated, and the login path then fails on a missing
`player.record` rather than skipping. So ask for the table itself.
""".
database_available(Service) ->
    Query = "SELECT to_regclass('player.record') IS NOT NULL AS present",
    try peer:call(Service, database, query, [auth_pool, Query]) of
        #{rows := [#{present := true}]} -> true;
        _Other -> false
    catch
        _:_ -> false
    end.

-doc """
A user nobody else will collide with.

`erlang:unique_integer/1` is not enough here: it restarts with the VM
and every `rebar3 ct` run is a new one, so consecutive runs against a
persistent database hand out the same names and the insert fails on the
second run only.
""".
unique_player_id() ->
    <<Id:63/unsigned-integer, _:1>> = crypto:strong_rand_bytes(8),
    Id.

purge_session(Service, PlayerId) ->
    Delete = "DELETE FROM player.session WHERE player_id = $1",
    _ = peer:call(Service, database, query, [lyceum_pool, Delete, [PlayerId]]),
    ok.

create_user(Service) ->
    Unique = binary_to_list(binary:encode_hex(crypto:strong_rand_bytes(8), lowercase)),
    Username = "e2e_" ++ Unique,
    Email = Username ++ "@example.com",
    Password = "hunter2",
    User = #{username => Username, email => Email, password => Password},
    ok = peer:call(Service, registry, insert_user, [User, auth_pool]),
    {Username, Email, Password}.
