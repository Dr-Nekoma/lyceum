-module(lyceum_dev).
-moduledoc """
A three-node Lyceum cluster on your machine, for development.

`just cluster` runs this. It uses braid to launch one VM per layer and
to keep them alive for as long as this shell is: braid's kill switch
means closing the shell takes the cluster with it, so a stray node
never survives to confuse the next run.

The layers are wired the same way a deployment wires them, through the
`peers` configuration each node reads at boot, not by braid's
`connections`. That is deliberate: `lyceum_cluster_connector` is what
production relies on, including its retry when a layer is not up yet,
so the dev cluster should exercise it rather than route around it. It
also means the nodes can be started in any order.

    just cluster

    > lyceum_dev:status().     %% what each node believes about the cluster
    > lyceum_dev:nodes().      %% who each node is connected to
    > braid:multicall(lyceum_dev:manager(), erlang, memory, [total]).

The frontend node has to be `lyceum_server`: that is the name the Zig
client builds from the address you type into it.
""".

-export([cluster/0, manager/0, stop/0]).
-export([status/0, nodes/0]).

-define(FRONTEND, lyceum_server).
-define(LOGIC, lyceum_logic).
-define(SERVICE, lyceum_svc).

%% Relative to the directory `just cluster` runs from, so the spawned
%% nodes load the same compiled code as this shell.
-define(CODE_PATH, "_build/dev/lib/*/ebin").

-doc """
Launches the cluster and returns braid's manager, which every other
function here takes.
""".
-spec cluster() -> pid().
cluster() ->
    Cluster = braid:create(node_map()),
    io:format(
        "~nLyceum cluster up:~n"
        "  ~p  frontend (the client connects here)~n"
        "  ~p  logic~n"
        "  ~p  service~n~n"
        "Try lyceum_dev:status() or lyceum_dev:nodes().~n"
        "Closing this shell stops all three.~n~n",
        [node_name(?FRONTEND), node_name(?LOGIC), node_name(?SERVICE)]
    ),
    Cluster.

-doc """
braid's manager, which it registers locally under `braid`. Everything
below goes through it, so a shell that did not call `cluster/0` itself
can still drive the cluster.
""".
-spec manager() -> pid().
manager() ->
    case whereis(braid) of
        undefined -> error(no_cluster_running);
        Pid -> Pid
    end.

-spec stop() -> ok.
stop() ->
    braid:stop(manager()).

-doc "Each node's own view of the cluster, straight from its connector.".
-spec status() -> map().
status() ->
    braid:multicall(manager(), lyceum_cluster_connector, status, []).

-doc """
Who each node is actually connected to. On a healthy cluster the
frontend sees only the logic node, and the service node likewise: if
the frontend can see the service node, the layering has been broken.
""".
-spec nodes() -> map().
nodes() ->
    braid:multicall(manager(), erlang, nodes, []).

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec node_map() -> map().
node_map() ->
    #{
        ?SERVICE => node_spec("service", [], [lyceum_service, world]),
        ?LOGIC => node_spec("logic", [?SERVICE], [player, world]),
        ?FRONTEND => node_spec("frontend", [?LOGIC], [auth])
    }.

-spec node_spec(string(), [atom()], [atom()]) -> map().
node_spec(Types, Peers, Apps) ->
    #{
        args => [
            {connect_all, false},
            "-setcookie lyceum",
            {pa, ?CODE_PATH},
            %% These nodes run from the source tree rather than a
            %% release, so migrations and queries are found relative to
            %% the directory braid was started in.
            "-database root_dir '\".\"'",
            config_flag(Types, Peers),
            {eval, start_apps(Apps)}
        ],
        %% Empty on purpose: the connector dials the layer below from
        %% the `peers` configuration above.
        connections => []
    }.

-doc """
The same `-App Key Value` form a node without a config file is
configured with, carrying the same strings `sys.config.src` would
produce from the environment.

The single quotes survive the shell braid spawns `erl` through, so the
value reaches Erlang as a quoted string rather than a bare atom.
""".
-spec config_flag(string(), [atom()]) -> string().
config_flag(Types, Peers) ->
    Names = string:join([atom_to_list(node_name(P)) || P <- Peers], ","),
    "-lyceum_cluster node_types '\"" ++ Types ++ "\"' peers '\"" ++ Names ++ "\"'".

-spec start_apps([atom()]) -> string().
start_apps(Apps) ->
    string:join(
        ["application:ensure_all_started(" ++ atom_to_list(App) ++ ")" || App <- Apps],
        ","
    ).

-spec node_name(atom()) -> node().
node_name(Name) ->
    {ok, Host} = inet:gethostname(),
    list_to_atom(atom_to_list(Name) ++ "@" ++ Host).
