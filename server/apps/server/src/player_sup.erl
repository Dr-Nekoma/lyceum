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
-spec start_link() -> supervisor:startlink_ret().
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

-spec start(term()) -> supervisor:startchild_ret().
start(Cache) ->
    logger:info("[~p] Starting CHILD with ARGS = ~p~n", [?SERVER, Cache]),
    supervisor:start_child(?MODULE, [Cache]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================
-spec init([]) -> {ok, term()}.
init([]) ->
    SupFlags =
        #{strategy => simple_one_for_one,
          intensity => 24,
          period => 3600},

    PlayerWorker =
        #{id => player,
          start => {player, start_link, []},
          restart => transient,
          shutdown => 300,
          type => worker,
          modules => [player]},

    logger:info("[~p] Starting Supervisor...~n", [?SERVER]),
    {ok, {SupFlags, [PlayerWorker]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
