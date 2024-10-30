%%%-------------------------------------------------------------------
%% @doc User top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(player_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start/1]).
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

start(Args) ->
    io:format("SUP ~p~n...", [Args]),
    supervisor:start_child(?MODULE, Args).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================
init([]) ->
    SupFlags =
        #{strategy => simple_one_for_one,
          intensity => 1,
          period => 5},

    PlayerWorker =
        #{id => player,
          start => {player, start_link, []},
          restart => transient,
          shutdown => brutal_kill,
          type => worker,
          modules => [player]},

    io:format("[~p] Starting Supervisor...~n", [?SERVER]),
    {ok, {SupFlags, [PlayerWorker]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
