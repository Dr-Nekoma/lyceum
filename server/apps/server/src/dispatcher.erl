%%%-------------------------------------------------------------------
%% @doc Dispatcher API
%% @end
%%%-------------------------------------------------------------------
-module(dispatcher).

-behaviour(gen_server).

%% API
-export([start_link/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-define(SERVER, lyceum_server).

-include("user_state.hrl").
-include("server_state.hrl").

-dialyzer({nowarn_function, [login/3, start/2, get_active_pid/2]}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> gen_server:start_ret().
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec init(Args) -> Result
    when Args :: list(),
         State ::
             #server_state{connection :: pid(),
                           pid :: pid(),
                           table :: atom() | ets:tid()},
         Success :: {ok, State},
         SuccessWithTimeout :: {ok, State, Timeout :: timeout()},
         Result :: Success | SuccessWithTimeout.
init([]) ->
    % Setup a DB connection and bootstrap process state
    {ok, Connection} = database:connect(),
    Pid = self(),
    logger:debug("[~p] Starting at ~p...~n", [?SERVER, Pid]),
    % This is a temporary solution using the built-in k/v store
    Table = ets:new(?MODULE, [named_table, private, set]),
    State =
        #server_state{connection = Connection,
                      pid = self(),
                      table = Table},
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec handle_call(term(), gen_server:from(), server_state()) -> Result
    when Result ::
             {reply, term(), server_state()} |
             {reply, term(), server_state(), timeout()} |
             {noreply, server_state()} |
             {noreply, server_state(), timeout()} |
             {stop, term(), term(), server_state()} |
             {stop, term(), server_state()}.
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(term(), server_state()) -> Result
    when Result ::
             {noreply, server_state()} |
             {noreply, server_state(), timeout()} |
             {stop, term(), server_state()}.
handle_cast({logout, Email}, State) ->
    ets:delete(?MODULE, Email),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec handle_info(term(), server_state()) -> Result
    when Result ::
             {noreply, server_state()} |
             {noreply, server_state(), timeout()} |
             {stop, term(), server_state()}.
handle_info({From, {login, Request}}, State) ->
    logger:info("[~p] INFO: ~p~n", [?SERVER, From]),
    login(State, From, Request),
    {noreply, State};
handle_info(Info, State) ->
    logger:error("[~p] INFO: ~p~n", [?SERVER, Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec terminate(term(), server_state()) -> ok.
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec code_change(term(), server_state(), term()) -> {ok, server_state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec login(server_state(), gen_server:from(), map()) -> ok.
login(State, From, #{username := Username, password := _Password} = Request) ->
    logger:info("[~p] User ~p is attempting to login from ~p~n", [?SERVER, Username, From]),
    case registry:check_user(Request, State#server_state.connection) of
        {ok, Email} ->
            logger:info("[~p] USER: ~p successfully logged in!~n", [?SERVER, Email]),
            {ok, Connection} = database:connect(),
            PlayerState = #user_state{pid = From, connection = Connection},
            logger:info("[~p] Setting USER_STATE=~p...~n", [?SERVER, PlayerState]),
            {ok, Pid} = get_active_pid(Email, PlayerState),
            logger:info("[~p] Successfully Spawned ~p~n", [?SERVER, Pid]),
            From ! {ok, {Pid, Email}};
        {error, Message} ->
            logger:error("Failed to login: ~p~n", [Message]),
            From ! {error, Message}
    end,
    ok.

-spec get_active_pid(term(), user_state()) -> {ok, pid()}.
get_active_pid(Email, State) ->
    case ets:lookup(?MODULE, Email) of
        [{_, Pid}] ->
            logger:info("[~p] Logging ~p at ~p...~n", [?SERVER, Email, Pid]),
            Pid ! logout,
            start(Email, State);
        _ ->
            start(Email, State)
    end.

-spec start(term(), user_state()) -> {ok, pid()}.
start(Email, State) ->
    {ok, NewPid} = player_sup:start([{Email, State}]),
    ets:insert(?MODULE, {Email, NewPid}),
    logger:info("[~p] <Email=~p, PID=~p> inserted...~n", [?SERVER, Email, NewPid]),
    {ok, NewPid}.
