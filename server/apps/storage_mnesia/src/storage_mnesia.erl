-module(storage_mnesia).
-behaviour(gen_server).

%% API
-export([start_link/0]).
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
    PlayerTableSettings =
        [{attributes, record_info(fields, ?PLAYER_CACHE_TABLE)},
         % Extra indexes to help certain queries later
         {index, [#player_cache.username, #player_cache.email]}],
    Options = PlayerTableSettings ++ DefaultAttributes,
    case mnesia:system_info(db_nodes) of
        [] ->
            mnesia:create_schema(Nodes);
        _ ->
            ok
    end,
    % Warning: You cant use "maybe" and enable Erlandono's
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
handle_call({login, CacheData}, _, State) ->
    NewData =
        case get_by_id(?PLAYER_CACHE_TABLE, CacheData#player_cache.player_id) of
            {ok, Data} ->
                Data#player_cache{client_pid = CacheData#player_cache.client_pid};
            {error, Reason} ->
                logger:warning("[~p] Cache missing: ~p~n", [?MODULE, Reason]),
                CacheData
        end,
    upsert_record(?PLAYER_CACHE_TABLE, NewData),
    Reply = {ok, NewData},
    {reply, Reply, State};
handle_call({get_by_id, UserId}, _, State) ->
    Reply = get_by_id(?PLAYER_CACHE_TABLE, UserId),
    {reply, Reply, State};
handle_call({get_by_username, Username}, _, State) ->
    Reply = get_by_username(?PLAYER_CACHE_TABLE, Username),
    {reply, Reply, State};
handle_call({get_by_email, Email}, _, State) ->
    Reply = get_by_email(?PLAYER_CACHE_TABLE, Email),
    {reply, Reply, State};
handle_call(_Request, _, State) ->
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
handle_cast({upsert_record, Cache}, State) ->
    upsert_record(?PLAYER_CACHE_TABLE, Cache),
    {noreply, State};
handle_cast({logout, UserId}, State) ->
    delete_player_entry(?PLAYER_CACHE_TABLE, UserId),
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
%% Takes a deterministacally generated UserID and
%% fetches the PID associated with it.
%% @end
-spec get_by_id(Table, UserId) -> Result
    when Table :: mnesia:table(),
         UserId :: player_id(),
         Cache :: player_cache(),
         Reason :: mnesia_query_error(),
         Ok :: {ok, Cache},
         Error :: {error, Reason},
         Result :: Ok | Error.
get_by_id(Table, UserId) ->
    Fun = fun() -> mnesia:read({Table, UserId}) end,
    format_query_result(mnesia:transaction(Fun)).

%% @doc
%% Query by a player's username, which is indexed by
%% MNESIA.
%% @end
-spec get_by_username(Table, Username) -> Result
    when Table :: mnesia:table(),
         Username :: player_name(),
         Cache :: player_cache(),
         Reason :: mnesia_query_error(),
         Ok :: {ok, Cache},
         Error :: {error, Reason},
         Result :: Ok | Error.
get_by_username(Table, Username) ->
    Fun = fun() ->
             QH = qlc:q([X || X <- mnesia:table(Table), X#player_cache.username =:= Username]),
             qlc:eval(QH)
          end,
    format_query_result(mnesia:transaction(Fun)).

%% @doc
%% Query by a player's email, which is indexed by
%% MNESIA.
%% @end
-spec get_by_email(Table, Email) -> Result
    when Table :: mnesia:table(),
         Email :: player_name(),
         Cache :: player_cache(),
         Reason :: mnesia_query_error(),
         Ok :: {ok, Cache},
         Error :: {error, Reason},
         Result :: Ok | Error.
get_by_email(Table, Email) ->
    Fun = fun() ->
             QH = qlc:q([X || X <- mnesia:table(Table), X#player_cache.email =:= Email]),
             qlc:eval(QH)
          end,
    format_query_result(mnesia:transaction(Fun)).

%% @doc
%% Takes a deterministacally generated UserID and
%% fetches the PID associated with it.
%% @end
-spec upsert_record(Table, Cache) -> Result
    when Table :: mnesia:table(),
         Cache :: player_cache(),
         Pid :: pid(),
         Ok :: {ok, Pid},
         Error :: {error, mnesia_write_error},
         Result :: Ok | Error.
upsert_record(Table, Cache) ->
    Fun = fun() -> mnesia:write(Table, Cache, write) end,
    case mnesia:transaction(Fun) of
        {atomic, ok} ->
            {ok, Cache#player_cache.client_pid};
        _ ->
            {error, mnesia_write_error}
    end.

%% @doc
%% Removes a UserID from the cache table, therefore
%% invalidating the PID associated with it.
%% @end
-spec delete_player_entry(Table, UserId) -> Result
    when Table :: mnesia:table(),
         UserId :: player_id(),
         Error :: {error, mnesia_delete_error},
         Ok :: {ok, UserId},
         Result :: Ok | Error.
delete_player_entry(Table, UserId) ->
    Fun = fun() -> mnesia:delete(Table, UserId, write) end,
    case mnesia:transaction(Fun) of
        {atomic, ok} ->
            {ok, UserId};
        _ ->
            {error, delete_write_error}
    end.
