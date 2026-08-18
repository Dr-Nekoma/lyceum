-module(lyceum_cluster_sup).
-moduledoc """
Top level supervisor for `lyceum_cluster`.

Uses `rest_for_one` so the `pg` scope is always running before the
connector: the connector registers this node in a process group as
part of its own `init/1`, and every restart of the scope invalidates
that registration, so the connector has to follow it down.
""".

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

-spec start_link() -> supervisor:startlink_ret().
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

-spec init(list()) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    SupFlags =
        #{
            strategy => rest_for_one,
            intensity => 12,
            period => 3600
        },

    % Owns the ETS table backing every group lookup. Everything else in
    % the umbrella depends on this being up.
    Scope =
        #{
            id => pg,
            start => {pg, start_link, [lyceum_cluster:scope()]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [pg]
        },

    Connector =
        #{
            id => lyceum_cluster_connector,
            start => {lyceum_cluster_connector, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [lyceum_cluster_connector]
        },

    logger:info("[~p] Starting Supervisor...~n", [?SERVER]),
    {ok, {SupFlags, [Scope, Connector]}}.
