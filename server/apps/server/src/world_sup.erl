%%%-------------------------------------------------------------------
%% @doc World supervisor.
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
        #{strategy => one_for_one,
          intensity => 12,
          period => 3600},

    WorldWorker =
        #{id => world,
          start => {world, start_link, []},
          restart => permanent,
          shutdown => brutal_kill,
          type => worker,
          modules => [world]},

    io:format("[~p] Starting Supervisor...~n", [?SERVER]),
    {ok, {SupFlags, [WorldWorker]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
