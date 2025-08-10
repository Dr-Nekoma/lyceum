-module(cache).
-behaviour(gen_server).

%% API
-export([start_link/0, get_by_id/1, login/1, logout/1, upsert_player_record/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-define(MNESIA_INIT_TIMEOUT, 30000).
-define(SERVER, ?MODULE).
-define(PLAYER_CACHE_TABLE, player_cache).

-include("mnesia_state.hrl").
-include("player_state.hrl").

-include_lib("stdlib/include/qlc.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> gen_server:start_ret().
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%% Fetchs Player's CLIENT PID from MNESIA.
%% @end
%%--------------------------------------------------------------------
-spec get_by_id(PlayerId) -> Result
    when
      PlayerId :: player_id(),
      State :: player_state(),
      Reply :: {reply, term(), State} | {reply, term(), State, timeout()},
      NoReply :: {noreply, State} | {noreply, State, timeout()},
      Stop :: {stop, term(), term(), State} | {stop, term(), State},
      Result :: Reply | NoReply | Stop.
get_by_id(PlayerId) ->
    gen_server:call(?MODULE, {get_by_id, PlayerId}).

%%--------------------------------------------------------------------
%% @doc
%% Attemps to login a Player
%% @end
%%--------------------------------------------------------------------
-spec login(Data) -> Result
    when
      Data :: player_cache(),
      State :: player_state(),
      Reply :: {reply, term(), State} | {reply, term(), State, timeout()},
      NoReply :: {noreply, State} | {noreply, State, timeout()},
      Stop :: {stop, term(), term(), State} | {stop, term(), State},
      Result :: Reply | NoReply | Stop.
login(Data) ->
    gen_server:call(?MODULE, {login, Data}).


%%--------------------------------------------------------------------
%% @doc
%% Updates a Player's PID.
%% @end
%%--------------------------------------------------------------------
-spec upsert_player_record(Cache) -> Result
    when
      Cache :: player_cache(),
      Reply :: {reply, term(), State} | {reply, term(), State, timeout()},
      NoReply :: {noreply, State} | {noreply, State, timeout()},
      Stop :: {stop, term(), term(), State} | {stop, term(), State},
      Result :: Reply | NoReply | Stop.
upsert_player_record(Cache) ->
    gen_server:call(?MODULE, {upsert_player_record, Cache}).

%%--------------------------------------------------------------------
%% @doc
%% Logs a Player off
%% @end
%%--------------------------------------------------------------------
logout(PlayerId) ->
    gen_server:cast(?MODULE, {logout, PlayerId}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
-spec init(Args) -> Result
    when Args :: list(),
         State :: mnesia_state(),
         Error :: {stop, Reason :: string()},
         Ok :: {ok, State},
         Result :: Ok | Error.
init(_) ->
    Nodes = [node()],
    DefaultAttributes = [{ram_copies, Nodes}],
    PlayerTableSettings = [{attributes, record_info(fields, ?PLAYER_CACHE_TABLE)}],
    Options = PlayerTableSettings ++ DefaultAttributes,
    case mnesia:system_info(db_nodes) of
        [] ->
            mnesia:create_schema(Nodes);
        _ ->
            ok
    end,
    % Warning: You can't use "maybe" and enable Erlandono's
    % "do" parse transform at the same time.
    maybe
        {ok, Connection} ?= database:connect_as_mnesia(),
        State = #mnesia_state{connection = Connection},
        ok ?= mnesia:start(),
        ok ?= create_table(?PLAYER_CACHE_TABLE, Options),
        ok ?= mnesia:wait_for_tables([?PLAYER_CACHE_TABLE], ?MNESIA_INIT_TIMEOUT),
        {ok, State}
    else
        {error, Reason} -> 
            logger:error("[~p] Error: ~p~n", [?MODULE, Reason]),
            {stop, Reason};
        Err ->
            logger:error("[~p] Unexpected Error: ~p~n", [?MODULE, Err]),
            {stop, "Unexpected Error"}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request, From, State) -> Result
    when 
      Request :: term(),
      From :: gen_server:from(),
      State :: mnesia_state(),
      Reply :: {reply, term(), State} | {reply, term(), State, timeout()},
      NoReply :: {noreply, State} | {noreply, State, timeout()},
      Stop :: {stop, term(), term(), State} | {stop, term(), State},
      Result :: Reply | NoReply | Stop.
handle_call({login, CacheData}, From, State) ->
    logger:debug("[~p] Player Logging From: ~p~n", [?MODULE, From]),
    NewData =
        case get_by_player_id(?PLAYER_CACHE_TABLE, CacheData#player_cache.player_id) of
            {ok, Data} ->
                %% TODO: 
                %% 1. Kill old PID
                %% 2. Replace old record
                Data#player_cache{client_pid = CacheData#player_cache.client_pid};
            {error, Reason} ->
                logger:warning("[~p] Cache missing: ~p~n", [?MODULE, Reason]),
                CacheData
        end,
    upsert_player_record(?PLAYER_CACHE_TABLE, NewData),
    Reply = {ok, NewData},
    {reply, Reply, State};
handle_call({get_by_id, UserId}, _, State) ->
    Reply = get_by_player_id(?PLAYER_CACHE_TABLE, UserId),
    {reply, Reply, State};
handle_call({upsert_record, Cache}, _, State) ->
    Reply = upsert_player_record(?PLAYER_CACHE_TABLE, Cache),
    {reply, Reply, State};
handle_call(_Request, _, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Msg, State) -> Result
    when
      Msg :: term(),
      State :: player_state(),
      Reason :: string(),
      Timeout :: gen_server:timeout(),
      NoReply :: {noreply, State} | {noreply, State, Timeout},
      Stop :: {stop, Reason, State},
      Result :: NoReply | Stop.
handle_cast({logout, UserId}, State) ->
    delete_player_record(?PLAYER_CACHE_TABLE, UserId),
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
handle_info(_Info, State) ->
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
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% @doc
%% An abstraction to avoid crashing if a table
%% already exists.
%% @end
-spec create_table(RecordType, Attributes) -> Result
    when RecordType :: mnesia:table(),
         Attributes :: [mnesia:create_option()],
         Ok :: ok,
         Error :: {error, Reason :: string()},
         Result :: Ok | Error.
create_table(RecordType, Attributes) ->
    case mnesia:create_table(RecordType, Attributes) of
        {atomic, ok} ->
            ok;
        {aborted, {already_exists, Table}} ->
            Message = io_lib:format("Table ~p already exists, skipping creation!~n", [Table]),
            logger:warning(Message),
            ok;
        {aborted, Reason} ->
            logger:error(Reason),
            {error, Reason}
    end.

%% @doc
%% Standardizes the output of MNESIA transactions:.
%% @end
-spec format_query_result(Result) -> Result
    when Result :: {atomic, term()} | {aborted, term()},
         Cache :: player_cache(),
         Reason :: mnesia_query_error(),
         Ok :: {ok, Cache},
         Error :: {error, Reason},
         Result :: Ok | Error.
format_query_result(Result) ->
    case Result of
        {atomic, []} ->
            {error, not_found};
        {atomic, [C]} ->
            {ok, C};
        {atomic, _} ->
            {error, inconsistent_data};
        _ ->
            {error, generic_error}
    end.

%% @doc
%% Takes a deterministacally generated PlayerId and
%% fetches the Client PID associated with it.
%% @end
-spec get_by_player_id(Table, PlayerId) -> Result
    when Table :: mnesia:table(),
         PlayerId :: player_id(),
         Cache :: player_cache(),
         Reason :: mnesia_query_error(),
         Ok :: {ok, Cache},
         Error :: {error, Reason},
         Result :: Ok | Error.
get_by_player_id(Table, PlayerId) ->
    Fun = fun() -> mnesia:read({Table, PlayerId}) end,
    format_query_result(mnesia:transaction(Fun)).

%% @doc
%% Upserts Cache data in a Player Table.
%% @end
-spec upsert_player_record(Table, Cache) -> Result
    when Table :: mnesia:table(),
         Cache :: player_cache(),
         Ok :: {ok, Cache},
         Error :: {error, mnesia_write_error},
         Result :: Ok | Error.
upsert_player_record(Table, Cache) ->
    Fun = fun() -> mnesia:write(Table, Cache, write) end,
    case mnesia:transaction(Fun) of
        {atomic, ok} ->
            logger:debug("[~p] RECORD ~p INSERTED ON ~p~n", [?MODULE, Cache, Table]),
            {ok, Cache};
        Err ->
            logger:error("[~p] ~p~n", [?MODULE, Err]),
            {error, mnesia_write_error}
    end.

%% @doc
%% Removes a UserID from the cache table, therefore
%% invalidating the PID associated with it.
%% @end
-spec delete_player_record(Table, UserId) -> Result
    when Table :: mnesia:table(),
         UserId :: player_id(),
         Error :: {error, mnesia_delete_error},
         Ok :: {ok, UserId},
         Result :: Ok | Error.
delete_player_record(Table, UserId) ->
    Fun = fun() -> mnesia:delete(Table, UserId, write) end,
    case mnesia:transaction(Fun) of
        {atomic, ok} ->
            {ok, UserId};
        _ ->
            {error, delete_write_error}
    end.
