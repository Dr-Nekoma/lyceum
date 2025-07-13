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
init([]) ->
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
    Connection = database:connect_as_mnesia(),
    State = #mnesia_state{connection = Connection},
    ok = mnesia:start(),
    ok = create_table(?PLAYER_CACHE_TABLE, Options),
    ok = mnesia:wait_for_tables([?PLAYER_CACHE_TABLE], ?MNESIA_INIT_TIMEOUT),
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
handle_call({login, CacheData}, _, State) ->
    Reply =
        case get_by_id(CacheData#player_cache.player_id) of
            {ok, Data} ->
                Data#player_cache{client_pid = CacheData#player_cache.client_pid};
            {error, _} ->
                CacheData
        end,
    {reply, Reply, State};
handle_call({get_by_id, UserId}, _, State) ->
    {ok, Reply} = get_by_id(UserId),
    {reply, Reply, State};
handle_call({get_by_username, Username}, _, State) ->
    {ok, Reply} = get_by_username(Username),
    {reply, Reply, State};
handle_call({get_by_email, Email}, _, State) ->
    {ok, Reply} = get_by_email(Email),
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
    upsert_record(Cache),
    {noreply, State};
handle_cast({logout, UserId}, State) ->
    delete_player_entry(UserId),
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
-spec get_by_id(UserId) -> Result
    when UserId :: player_id(),
         Cache :: player_cache(),
         Reason :: mnesia_query_error(),
         Ok :: {ok, Cache},
         Error :: {error, Reason},
         Result :: Ok | Error.
get_by_id(UserId) ->
    Fun = fun() -> mnesia:read({player_cache, UserId}) end,
    format_query_result(mnesia:transaction(Fun)).

%% @doc
%% Query by a player's username, which is indexed by
%% MNESIA.
%% @end
-spec get_by_username(Username) -> Result
    when Username :: player_name(),
         Cache :: player_cache(),
         Reason :: mnesia_query_error(),
         Ok :: {ok, Cache},
         Error :: {error, Reason},
         Result :: Ok | Error.
get_by_username(Username) ->
    Fun = fun() ->
             QH = qlc:q([X
                         || X <- mnesia:table(?PLAYER_CACHE_TABLE),
                            X#player_cache.username =:= Username]),
             qlc:eval(QH)
          end,
    format_query_result(mnesia:transaction(Fun)).

%% @doc
%% Query by a player's email, which is indexed by
%% MNESIA.
%% @end
-spec get_by_email(Email) -> Result
    when Email :: player_name(),
         Cache :: player_cache(),
         Reason :: mnesia_query_error(),
         Ok :: {ok, Cache},
         Error :: {error, Reason},
         Result :: Ok | Error.
get_by_email(Email) ->
    Fun = fun() ->
             QH = qlc:q([X
                         || X <- mnesia:table(?PLAYER_CACHE_TABLE),
                            X#player_cache.email =:= Email]),
             qlc:eval(QH)
          end,
    format_query_result(mnesia:transaction(Fun)).

%% @doc
%% Takes a deterministacally generated UserID and
%% fetches the PID associated with it.
%% @end
-spec upsert_record(Cache) -> Result
    when Cache :: player_cache(),
         Pid :: pid(),
         Ok :: {ok, Pid},
         Error :: {error, mnesia_write_error},
         Result :: Ok | Error.
upsert_record(Cache) ->
    Fun = fun() -> mnesia:write(player_cache, Cache, write) end,
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
-spec delete_player_entry(UserId) -> Result
    when UserId :: player_id(),
         Error :: {error, mnesia_delete_error},
         Ok :: {ok, UserId},
         Result :: Ok | Error.
delete_player_entry(UserId) ->
    Fun = fun() -> mnesia:delete(metric, UserId, write) end,
    case mnesia:transaction(Fun) of
        {atomic, ok} ->
            {ok, UserId};
        _ ->
            {error, delete_write_error}
    end.
