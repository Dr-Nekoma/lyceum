-module(world_sup).
-moduledoc """
World supervisor.

Uses rest_for_one so the world process only starts after the migration
runner, and gets restarted whenever the migration runner crashes (the
world is only meaningful on top of a migrated schema).

The two children belong to different layers: migrations own the
database schema and so run on `service`, while the world itself is a
pure in-memory process serving player state machines and runs on
`logic`. On an all-in-one node both are present and the ordering above
still holds; on a logic-only node the world starts on its own, trusting
that some service node has migrated the schema.
""".

-behaviour(supervisor).

%% API
-export([start_link/0]).
%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

-spec start_link() -> supervisor:startlink_ret().
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================
-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    SupFlags =
        #{
            strategy => rest_for_one,
            intensity => 12,
            period => 3600
        },

    % Runs the DB migrations (main -> repeatable -> init -> test),
    % retrying until the database is reachable.
    Migrations =
        #{
            id => world_migrations,
            start => {world_migrations, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [world_migrations]
        },

    WorldWorker =
        #{
            id => world,
            start => {world, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [world]
        },

    Specs = [{service, Migrations}, {logic, WorldWorker}],
    Children = [Spec || {Layer, Spec} <- Specs, lyceum_cluster:hosts_layer(Layer)],

    logger:info("[~p] Starting Supervisor...~n", [?SERVER]),
    {ok, {SupFlags, Children}}.
