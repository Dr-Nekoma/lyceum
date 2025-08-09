%%%-------------------------------------------------------------------
%% @doc The Cache Application Top Level Supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(cache_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).
%% Supervisor callbacks
-export([init/1]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%% @end
%%--------------------------------------------------------------------
start_link() ->
    logger:info("[~p] Starting Supervisor...~n", [?MODULE]),
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%====================================================================
%% Supervisor Callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% @doc
%% Starts the Caching Application Top Level Supervisor.
%% @end
%%--------------------------------------------------------------------
init([]) ->
    SupFlags =
        #{strategy => one_for_one,
          intensity => 10,
          period => 600},

    CacheWorker =
        #{id => cache,
          start => {cache, start_link, []},
          restart => permanent,
          shutdown => 3000,
          type => worker,
          modules => [cache]},

    {ok, {SupFlags, [CacheWorker]}}.

%% internal functions
