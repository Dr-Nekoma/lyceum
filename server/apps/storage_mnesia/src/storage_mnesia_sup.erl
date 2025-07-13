%%%-------------------------------------------------------------------
%% @doc storage_mnesia top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(storage_mnesia_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    logger:info("[~p] Starting Supervisor...~n", [?MODULE]),
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags =
        #{strategy => one_for_one,
          intensity => 10,
          period => 600},

    StorageMNESIA =
        #{id => storage_mnesia,
          start => {storage_mnesia, start_link, []},
          restart => permanent,
          shutdown => 600,
          type => worker,
          modules => [storage_mnesia]},

    {ok, {SupFlags, [StorageMNESIA]}}.

%% internal functions
