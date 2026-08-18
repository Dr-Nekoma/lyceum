-module(cache).
-moduledoc """
The live-session store: who is logged in, and where their client is.

This was a mnesia table owned by a single gen_server on a single
service node. Being the only process that could answer a login is what
made "a player has at most one live session" true, and it is also what
capped the cluster at one service node.

It is now a plain module -- no process, no state -- executing SQL in
whichever `lyceum_service_worker` called it. That matters: `pgo` binds
a transaction to the calling process, so running here keeps the whole
login on the node that owns the pool.

The invariant survives the loss of the single process because
`player.session` is a PostgreSQL 18 temporal table: its primary key is
`(player_id, valid_at WITHOUT OVERLAPS)`, so two overlapping sessions
for one player are refused by the database no matter how many service
nodes are asking. A transaction-scoped advisory lock on the player serialises the
writers, and the constraint is the backstop that would catch a writer
which somehow skipped it.

Sessions are closed, never deleted, so the table is also the login
history.
""".

%% Session API
-export([login/2, logout/1, get_by_id/1]).
%% Reaping, used by session_reaper
-export([close_sessions_of_node/1, stale_sessions/2, close_stale_session/3]).

-include("player_state.hrl").

-define(POOL, lyceum_pool).
-define(QUERY_DIR, "session").

%% 23P01 exclusion_violation, 23505 unique_violation. Both mean another
%% node opened a session for this player first.
-define(OVERLAP_CODES, [<<"23P01">>, <<"23505">>]).

-type error() :: {error, term()}.

%%%===================================================================
%%% API
%%%===================================================================

-doc """
Opens a session for `Cache`, closing whatever session that player had.

Returns the previous session's client pid so the caller can kick it, or
`undefined` when there was none. `OwnerNode` is the logic node running
the player FSM: it is the only node the service layer can watch, and so
the key the reaper cleans up by.
""".
-spec login(player_cache(), node()) ->
    {ok, player_cache(), pid() | undefined} | error().
login(Cache, OwnerNode) ->
    login(Cache, OwnerNode, 1).

-doc "Closes the player's open session. A player with none is not an error.".
-spec logout(player_id()) -> ok | error().
logout(PlayerId) ->
    run(fun() ->
        _ = query("close_open_session.sql", [PlayerId]),
        ok
    end).

-doc "The player's open session, if there is one.".
-spec get_by_id(player_id()) -> {ok, player_cache()} | {error, not_found | term()}.
get_by_id(PlayerId) ->
    run(fun() ->
        case query("select_session.sql", [PlayerId]) of
            [] -> {error, not_found};
            [Row | _] -> {ok, row_to_cache(Row)}
        end
    end).

-doc """
Closes every open session owned by `Node`.

Called when a logic node goes down: the player FSMs it held are gone,
so their sessions are over even though nothing logged out.
""".
-spec close_sessions_of_node(node()) -> ok | error().
close_sessions_of_node(Node) ->
    run(fun() ->
        _ = query("close_sessions_by_node.sql", [Node]),
        ok
    end).

-doc """
Up to `Limit` open sessions that have been open longer than
`StaleAfterSeconds`, oldest first. Candidates only: whether they are
really stale depends on their owner node, which is the caller's
question to answer.
""".
-spec stale_sessions(non_neg_integer(), pos_integer()) ->
    {ok, [{player_id(), node()}]} | error().
stale_sessions(StaleAfterSeconds, Limit) ->
    run(fun() ->
        Rows = query("select_stale_sessions.sql", [StaleAfterSeconds, Limit]),
        {ok, [
            {maps:get(player_id, Row), to_atom(maps:get(owner_node, Row))}
         || Row <- Rows
        ]}
    end).

-doc """
Closes one stale session, re-checking staleness in the statement itself.

The check is repeated rather than trusted from `stale_sessions/2`
because the row can stop being stale in between -- the owner node
reconnects, or the player logs in again and this is a different session
entirely. Returns how many rows it actually closed, which is 0 when
that happened.
""".
-spec close_stale_session(player_id(), node(), non_neg_integer()) ->
    {ok, non_neg_integer()} | error().
close_stale_session(PlayerId, OwnerNode, StaleAfterSeconds) ->
    run(fun() ->
        {ok, affected_rows("close_stale_session.sql", [PlayerId, OwnerNode, StaleAfterSeconds])}
    end).

%%%===================================================================
%%% Login
%%%===================================================================

-spec login(player_cache(), node(), non_neg_integer()) ->
    {ok, player_cache(), pid() | undefined} | error().
login(Cache, OwnerNode, RetriesLeft) ->
    try transaction(fun() -> do_login(Cache, OwnerNode) end) of
        Result -> Result
    catch
        throw:{?MODULE, Reason} ->
            case overlap_error(Reason) andalso RetriesLeft > 0 of
                true ->
                    %% Another node won the race to open the first
                    %% session. Retry: there is a row to lock now, so
                    %% this becomes an ordinary duplicate login.
                    logger:info("[~p] Lost the login race, retrying~n", [?MODULE]),
                    login(Cache, OwnerNode, RetriesLeft - 1);
                false ->
                    logger:error("[~p] Login failed: ~p~n", [?MODULE, Reason]),
                    {error, Reason}
            end
    end.

-spec do_login(player_cache(), node()) -> {ok, player_cache(), pid() | undefined}.
do_login(#player_cache{player_id = PlayerId} = Cache, OwnerNode) ->
    %% Take the player's lock before looking: `FOR UPDATE` cannot
    %% serialise two nodes opening a *first* session, because there is
    %% no row to lock yet and both would insert. The advisory lock has
    %% no such gap, and being transaction-scoped it is released by the
    %% commit or rollback with no bookkeeping here.
    _ = query("lock_player.sql", [PlayerId]),
    Previous = open_session_client_pid(PlayerId),
    _ = query("close_open_session.sql", [PlayerId]),
    _ = query("open_session.sql", [
        PlayerId,
        Cache#player_cache.username,
        Cache#player_cache.email,
        term_to_binary(Cache#player_cache.client_pid),
        node(Cache#player_cache.client_pid),
        OwnerNode
    ]),
    {ok, Cache, Previous}.

-spec open_session_client_pid(player_id()) -> pid() | undefined.
open_session_client_pid(PlayerId) ->
    case query("select_open_session.sql", [PlayerId]) of
        [] -> undefined;
        [#{client_pid := Encoded} | _] -> decode_pid(Encoded)
    end.

%%%===================================================================
%%% Internal
%%%===================================================================

-doc """
Runs `Fun`, turning the throws `query/2` uses for control flow back
into `{error, _}` values. Nothing above the service layer sees an
exception from here.
""".
-spec run(fun(() -> Result)) -> Result | error().
run(Fun) ->
    try
        Fun()
    catch
        throw:{?MODULE, Reason} ->
            logger:error("[~p] Session query failed: ~p~n", [?MODULE, Reason]),
            {error, Reason}
    end.

-spec transaction(fun(() -> Result)) -> Result.
transaction(Fun) ->
    case database:transaction(?POOL, Fun) of
        {error, Reason} -> throw({?MODULE, Reason});
        Result -> Result
    end.

-doc """
Runs a named query, throwing on failure.

Throwing rather than returning is deliberate: inside
`database:transaction/2` an exception is what makes `pgo` roll back.
Returning an error value would let a half-finished login commit.
""".
-spec query(string(), [term()]) -> [map()].
query(File, Params) ->
    #{rows := Rows} = execute(File, Params),
    Rows.

-spec affected_rows(string(), [term()]) -> non_neg_integer().
affected_rows(File, Params) ->
    maps:get(num_rows, execute(File, Params), 0).

-spec execute(string(), [term()]) -> database:query_result().
execute(File, Params) ->
    SQL = database_queries:fetch_query(?QUERY_DIR, File),
    case database:query(?POOL, SQL, Params) of
        #{rows := _} = Result -> Result;
        {error, Reason} -> throw({?MODULE, Reason})
    end.

-spec overlap_error(term()) -> boolean().
overlap_error({pgo_error, Fields}) ->
    lists:member(error_code(Fields), ?OVERLAP_CODES);
overlap_error(_Other) ->
    false.

-spec error_code(map() | [{atom(), term()}]) -> binary() | undefined.
error_code(Fields) when is_map(Fields) ->
    maps:get(code, Fields, undefined);
error_code(Fields) when is_list(Fields) ->
    proplists:get_value(code, Fields, undefined);
error_code(_Other) ->
    undefined.

-spec row_to_cache(map()) -> player_cache().
row_to_cache(#{
    player_id := PlayerId,
    username := Username,
    email := Email,
    client_pid := Encoded
}) ->
    #player_cache{
        player_id = PlayerId,
        username = to_list(Username),
        email = to_list(Email),
        client_pid = decode_pid(Encoded)
    }.

-doc """
Decodes a stored pid.

`safe` is not usable here: the frontend node that owns the pid may be
one this node has never spoken to, so its name is not yet an atom, and
`safe` refuses to create it. The data is ours -- nothing but `login/2`
ever writes this column.
""".
-spec decode_pid(binary()) -> pid() | undefined.
decode_pid(Encoded) when is_binary(Encoded) ->
    try binary_to_term(Encoded) of
        Pid when is_pid(Pid) -> Pid;
        _Other -> undefined
    catch
        _:_ -> undefined
    end;
decode_pid(_Other) ->
    undefined.

-spec to_list(binary() | string()) -> string().
to_list(Value) when is_binary(Value) -> binary_to_list(Value);
to_list(Value) -> Value.

-spec to_atom(binary() | atom() | string()) -> atom().
to_atom(Value) when is_atom(Value) -> Value;
to_atom(Value) when is_binary(Value) -> binary_to_atom(Value, utf8);
to_atom(Value) when is_list(Value) -> list_to_atom(Value).
