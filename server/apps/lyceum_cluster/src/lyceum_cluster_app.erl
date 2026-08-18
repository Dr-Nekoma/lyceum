-module(lyceum_cluster_app).
-moduledoc """
Application callback for `lyceum_cluster`.

Every Lyceum node boots this application first, regardless of which
layer it belongs to. It is the only place that decides what kind of
node this VM is.
""".

-behaviour(application).

-export([start/2, stop/1]).

-doc """
Resolves the node's layers before starting the supervision tree.

`lyceum_cluster:node_types/0` raises when `node_types` is unset, which
fails the boot here rather than at the first service lookup. A node
that does not declare its layers is a configuration bug, not a
condition to recover from.
""".
-spec start(application:start_type(), term()) -> {ok, pid()} | {error, term()}.
start(_StartType, _StartArgs) ->
    Types = lyceum_cluster:node_types(),
    logger:info(
        "[~p] Starting Application, this node hosts ~p~n",
        [?MODULE, Types]
    ),
    lyceum_cluster_sup:start_link().

-spec stop(term()) -> ok.
stop(_State) ->
    ok.
