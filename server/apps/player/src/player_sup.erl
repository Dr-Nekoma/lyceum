%%%-------------------------------------------------------------------
%% @doc Player supervisor.
%% @end
%%%-------------------------------------------------------------------
-module(player_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).
%% Supervisor callbacks
-export([init/1]).

-include("player_state.hrl").

%%%===================================================================
%%% API functions
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%% @end
%%--------------------------------------------------------------------
%%-spec start_link() -> supervisor:startlink_ret().
start_link(Args) ->
    logger:info("[~p] Starting CHILD with ARGS = ~p~n", [?MODULE, Args]),
    Username = Args#player_cache.username,
    supervisor:start_link({global, Username}, ?MODULE, Args).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================
-spec init(PlayerCache) -> Result
    when PlayerCache :: player_cache(),
         Flags :: supervisor:sup_flags(),
         ChildSpecs :: [supervisor:child_spec()],
         Result :: {ok, {Flags, ChildSpecs}}.
init(PlayerCache) ->
    SupFlags =
        #{strategy => one_for_one,
          intensity => 10,
          period => 60,
          auto_shutdown => all_significant},

    PlayerWorker =
        #{id => player,
          start => {player, start_link, [PlayerCache]},
          restart => transient,
          % Defines if a child is considered significant
          % for automatic self-shutdown of the supervisor.
          significant => true,
          shutdown => 3000,
          type => worker,
          modules => [player]},

    {ok, {SupFlags, [PlayerWorker]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
