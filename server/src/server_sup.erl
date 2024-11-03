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
          intensity => 12,
          period => 3600},

    DispatcherSup =
        #{id => dispatcher_sup,
          start => {dispatcher_sup, start_link, []},
          restart => permanent,
          shutdown => 5000,
          type => supervisor,
          modules => [dispatcher_sup]},

    PlayerSup =
        #{id => player_sup,
          start => {player_sup, start_link, []},
          restart => permanent,
          shutdown => 5000,
          type => supervisor,
          modules => [player_sup]},

    io:format("[~p] Starting Top Level Supervisor...~n", [?SERVER]),
    {ok, {SupFlags, [DispatcherSup, PlayerSup]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
