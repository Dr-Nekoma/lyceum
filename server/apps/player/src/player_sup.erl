%%%-------------------------------------------------------------------
%% @doc User top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(player_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).
%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

-include("player_state.hrl").

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%% @end
%%--------------------------------------------------------------------
-spec start_link(Args) -> Return
    when Args :: player_cache(),
         Return :: supervisor:startlink_ret().
start_link(Args) ->
    logger:debug("[~p] Starting CHILD with ARGS = ~p~n", [?MODULE, Args]),
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
          % If this supervisor has no (significant) children
          % then it's ok to shut it down.
          auto_shutdown => all_significant},

    PlayerWorker =
        #{id => player,
          start => {player, start_link, [PlayerCache]},
          restart => transient,
          % Defines if a child is considered significant
          % for automated shutdown of the supervisor.
          significant => true,
          shutdown => 3000,
          type => worker,
          modules => [player]},

    {ok, {SupFlags, [PlayerWorker]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
