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
    logger:info("[~p] Starting TOP SUPERVISOR...", [?MODULE]),
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
    logger:info("[~p] Starting CHILD SUPERVISOR with ARGS = ~p~n", [?MODULE, PlayerData]),
    case supervisor:start_child(?MODULE, [PlayerData]) of
        {ok, SupPid} ->
            {ok, SupPid};
        {ok, SupPid, _} ->
            {ok, SupPid};
        {error, Err} ->
            logger:error("[~p] start_child ERROR ~p~n", [?MODULE, Err]),
            {error, "Error while starting child supervisor"}
    end.

%%--------------------------------------------------------------------
%% @doc
%% Given an inner Player supervisor, find the PID of a child process
%% under a particular module.
%% @end
%%--------------------------------------------------------------------
% -spec find_child_pid(SupId, Module) -> Result
%     when SupId :: supervisor:sup_ref(),
%          Module :: module(),
%          Pid :: pid(),
%          Reason :: string(),
%          Ok :: {ok, Pid},
%          Error :: {error, Reason},
%          Result :: Ok | Error.
% find_child_pid(SupId, Module) ->
%     logger:debug("[~p] FIND ~p with ~p~n", [?MODULE, SupId, Module]),
%     Children = supervisor:which_children(SupId),
%     Fun = fun({Id, Pid, Type, Modules}) ->
%              logger:debug("[~p] Child ID: ~p, PID: ~p, Type: ~p, Modules: ~p~n",
%                           [?MODULE, Id, Pid, Type, Modules]),
%              case Modules of
%                  [Mod] when Mod =:= Module -> true;
%                  _ -> false
%              end
%           end,
%     case lists:filter(Fun, Children) of
%         [{_Id, Pid, _Type, _Modules}] ->
%             {ok, Pid};
%         _ ->
%             {error, "todo"}
%     end.

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
          period => 5},

    InternalSup =
        #{id => player_sup,
          start => {player_sup, start_link, Args},
          restart => transient,
          shutdown => 5000,
          type => supervisor,
          modules => [player_sup]},

    {ok, {SupFlags, [InternalSup]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
