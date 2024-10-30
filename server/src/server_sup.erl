%%%-------------------------------------------------------------------
%% @doc Server top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(server_sup).

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

    ServerWorker =
        #{id => server,
          start => {server, start_link, []},
          restart => permanent,
          shutdown => brutal_kill,
          type => worker,
          modules => [server]},

    io:format("[~p] Starting Supervisor...~n", [?SERVER]),
    {ok, {SupFlags, [ServerWorker]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
