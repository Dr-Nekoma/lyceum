%%%-------------------------------------------------------------------
%% @doc A simple module for Login/Pass authentication
%% @end
%%%-------------------------------------------------------------------
-module(simple_auth).

-behaviour(gen_server).

%% API
-export([start_link/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

% For legacy reasons, the client needs to send the first
% request to a "lyceum_server", which got broken into
% multiple processes. Nowadays we only need it for initial
% commnunications with this dispatcher gen_server.
-define(SERVER, lyceum_server).

-include("auth_state.hrl").
-include("player_state.hrl").

-dialyzer({nowarn_function, [login/3]}).

-compile({parse_transform, do}).

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
         State :: auth_state(),
         Success :: {ok, State},
         SuccessWithTimeout :: {ok, State, Timeout :: timeout()},
         Result :: Success | SuccessWithTimeout.
init(_) ->
    Pid = self(),
    logger:info("[~p] Starting at ~p...~n", [?SERVER, Pid]),
    {ok, Connection} = database:connect_as_auth(),
    State = #auth_state{connection = Connection, pid = Pid},
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%% @end
%%--------------------------------------------------------------------
-spec handle_call(term(), gen_server:from(), auth_state()) -> Result
    when Result ::
             {reply, term(), auth_state()} |
             {reply, term(), auth_state(), timeout()} |
             {noreply, auth_state()} |
             {noreply, auth_state(), timeout()} |
             {stop, term(), term(), auth_state()} |
             {stop, term(), auth_state()}.
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
-spec handle_cast(UserId, auth_state()) -> Result
    when UserId :: player_id(),
         Result ::
             {noreply, auth_state()} |
             {noreply, auth_state(), timeout()} |
             {stop, term(), auth_state()}.
handle_cast({logout, UserId}, State) ->
    gen_server:cast(storage_mnesia, {logout, UserId}),
    {noreply, State};
handle_cast(Msg, State) ->
    logger:error("[~p] CAST: ~p~n", [?SERVER, Msg]),
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
-spec handle_info(term(), auth_state()) -> Result
    when Result ::
             {noreply, auth_state()} |
             {noreply, auth_state(), timeout()} |
             {stop, term(), auth_state()}.
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
-spec terminate(term(), auth_state()) -> ok.
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
-spec code_change(term(), auth_state(), term()) -> {ok, auth_state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% Logs a user from the Zig Client, but first it checks whether the
%% PID (From) is already on MNESIA. Needs to return something in an
%% Result-like fashion {ok, Map} or {error, Reason} so the client
%% can properly parse it.
%% @end
%%--------------------------------------------------------------------
-spec login(State, From, Map) -> Result
    when State :: auth_state(),
         From :: gen_server:from(),
         Username :: player_name(),
         Password :: nonempty_string(),
         Map :: #{username := Username, password := Password},
         Pid :: pid(),
         Email :: player_email(),
         Ok :: {ok, {Pid, Email}},
         Error :: {error, Reason :: string()},
         Result :: Ok | Error.
login(State, From, #{username := Username, password := _Password} = Request) ->
    logger:info("[~p] User ~p is attempting to login from ~p~n", [?SERVER, Username, From]),
    case registry:check_user(Request, State#auth_state.connection) of
        {ok, {PlayerId, Email}} ->
            Cache =
                #player_cache{player_id = PlayerId,
                              client_pid = From,
                              username = Username,
                              email = Email},
            {ok, Pid} = start_new_worker(Cache),
            logger:info("[~p] USER: ~p successfully logged at ~p!~n", [?SERVER, Email, Pid]),
            Reply = {ok, {Pid, Email}},
            From ! Reply,
            Reply;
        {error, Message} ->
            logger:error("Failed to login: ~p~n", [Message]),
            Reply = {error, Message},
            From ! Reply,
            Reply
    end.

%%--------------------------------------------------------------------
%% @doc
%% Gets the PID of a player worker process from MNESIA (if it actually
%% exists). If it does, logoff the user and create a new process with
%% the a new Worker PID, then sends it back to Zig Client.
%% @end
%%--------------------------------------------------------------------
-spec start_new_worker(Cache) -> Result
    when Cache :: player_cache(),
         WorkerPid :: pid(),
         Result :: {ok, WorkerPid} | {error, Reason :: string()}.
start_new_worker(Cache) ->
    do([error_m
        || Request = {login, Cache},
           % TODO improve this
           {ok, Data} = gen_server:call(storage_mnesia, Request),
           logger:info("[~p] USER: ~p~n", [?SERVER, Data]),
           case player_sup:start(Data) of
               {ok, Pid} ->
                   return(Pid);
               _ ->
                   fail("Failed to start worker")
           end]).
