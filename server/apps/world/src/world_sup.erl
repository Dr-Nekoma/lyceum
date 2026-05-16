%%%-------------------------------------------------------------------
%% @doc World supervisor. Uses rest_for_one so the world process only
%%      starts after the migration runner, and gets restarted whenever
%%      the migration runner crashes (the world is only meaningful on
%%      top of a migrated schema).
%% @end
%%%-------------------------------------------------------------------

-module(world_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).
%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================
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

    logger:info("[~p] Starting Supervisor...~n", [?SERVER]),
    {ok, {SupFlags, [Migrations, WorldWorker]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
