%%%-------------------------------------------------------------------
%% @doc Dispatcher supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(dispatcher_sup).

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
          intensity => 1,
          period => 5},

    DispatcherWorker =
        #{id => dispatcher,
          start => {dispatcher, start_link, []},
          restart => permanent,
          shutdown => brutal_kill,
          type => worker,
          modules => [dispatcher]},

    io:format("[~p] Starting Supervisor...~n", [?SERVER]),
    {ok, {SupFlags, [DispatcherWorker]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
