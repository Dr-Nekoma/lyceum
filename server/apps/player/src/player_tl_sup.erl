-module(player_tl_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_child/1]).
%% Supervisor callbacks
-export([init/1]).

-include("player_state.hrl").

%%====================================================================
%% API functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> supervisor:startlink_ret().
start_link() ->
    logger:debug("[~p] Starting TOP LEVEL SUPERVISOR...~n", [?MODULE]),
    supervisor:start_link({global, ?MODULE}, ?MODULE, []).

%%--------------------------------------------------------------------
%% @doc
%% Starts a Player Supervisor, which will then start a chat server and
%% a player state machine.
%% @end
%%--------------------------------------------------------------------
-spec start_child(PlayerData) -> Result
    when PlayerData :: player_cache(),
         Pid :: pid(),
         Reason :: string(),
         Ok :: {ok, Pid},
         Error :: {error, Reason},
         Result :: Ok | Error.
start_child(PlayerData) ->
    logger:debug("[~p] Starting CHILD SUPERVISOR with ARGS = ~p~n", [?MODULE, PlayerData]),
    SupRef = {global, ?MODULE},
    % Specs = get_specs([PlayerData]),
    case supervisor:start_child(SupRef, [PlayerData]) of
        {ok, SupPid} ->
            {ok, SupPid};
        {ok, SupPid, _} ->
            {ok, SupPid};
        {error, {already_started, SupPid}} ->
            logger:notice("[~p] SUP ~p IS ALREADY STARTED~n", [?MODULE, SupPid]),
            {ok, SupPid};
        {error, Err} ->
            logger:error("[~p] start_child ERROR ~p~n", [?MODULE, Err]),
            {error, "Error while starting child supervisor"}
    end.

%%====================================================================
%% Supervisor callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% @doc
%% Starts a Dynamic Top Level Supervisor, a supervisor of supervisors.
%% @end
%%--------------------------------------------------------------------
-spec init(Args) -> Result
    when Args :: list(),
         Flags :: {supervisor:sup_flags(), list()},
         ChildSpecs :: [supervisor:child_spec()],
         Result :: {ok, {Flags, ChildSpecs}}.
init(Args) ->
    SupFlags =
        #{strategy => simple_one_for_one,
          % Don't restart children when
          % they terminate normally
          intensity => 0,
          % Max secs. between restarts
          period => 5,
          % This Top Level Supervisor will
          % live as long the VM is on
          auto_shutdow => never},

    InternalSup = get_specs(Args),

    logger:debug("[~p] TOP LEVEL SUPERVISOR STARTED AT ~p with ~p...",
                 [?MODULE, self(), InternalSup]),

    {ok, {SupFlags, [InternalSup]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
get_specs(Args) ->
    #{id => player_sup,
      start => {player_sup, start_link, Args},
      restart => transient,
      shutdown => 5000,
      type => supervisor,
      modules => [player_sup]}.
