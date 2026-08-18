%%%-------------------------------------------------------------------
%% @doc Runs the database migrations the world depends on, in order:
%%      main -> repeatable -> init -> test.
%%
%%      Following "Erlang in Anger": init/1 never blocks on (nor crashes
%%      because of) an external service. The connection attempt happens
%%      asynchronously via handle_continue/2 and is retried with capped
%%      exponential backoff until the database is reachable, so a slow or
%%      absent PostgreSQL cannot burn the supervisor's restart intensity
%%      at boot. The migrator connection is opened, used and closed within
%%      a single pass. No other process is ever bound to it.
%%
%%      A genuine migration failure (DB reachable but SQL fails) still
%%      crashes this process so the supervisor can act on it.
%% @end
%%%-------------------------------------------------------------------
-module(world_migrations).

-behaviour(gen_server).

%% API
-export([start_link/0, status/0, run_migrations/1]).
%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_continue/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-define(SERVER, ?MODULE).
%% Base delay between connection attempts, doubled on every retry
%% up until ?MAX_RETRY_MS.
-define(BASE_RETRY_MS, 1000).
-define(MAX_RETRY_MS, 30000).
%% How long to wait for the pgo pool to answer before giving up on this
%% pass and retrying the whole thing.
-define(POOL_READY_TIMEOUT_MS, 5000).
-define(POOL_READY_INTERVAL_MS, 50).
%% The pool map_generator seeds through; it hardcodes the same one.
-define(POOL, lyceum_pool).

-include("migrations.hrl").

-record(state, {status = waiting :: waiting | done, attempts = 0 :: non_neg_integer()}).

-type state() :: #state{}.

%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% Starts the migration runner
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> gen_server:start_ret().
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%% Returns whether the migrations already ran (waiting | done).
%% @end
%%--------------------------------------------------------------------
-spec status() -> waiting | done.
status() ->
    gen_server:call(?SERVER, status).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns immediately, the actual work is done in handle_continue/2
%% so supervisor start-up is never blocked by the database.
%%
%% Exits must be trapped here: epgsql links the connection process to
%% its caller, so a refused connection would otherwise kill us through
%% the link right after epgsql:connect returns {error, _}.
%% @end
%%--------------------------------------------------------------------
-spec init(Args) -> Result when
    Args :: list(),
    Result :: {ok, state(), {continue, migrate}}.
init([]) ->
    _ = process_flag(trap_exit, true),
    {ok, #state{}, {continue, migrate}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Tries to open the short-lived migrator connection. If the database
%% is unreachable, schedules a retry instead of crashing. Once it is
%% reachable, runs every migration in order and closes the connection.
%% @end
%%--------------------------------------------------------------------
-spec handle_continue(migrate, state()) -> {noreply, state()}.
handle_continue(migrate, #state{status = done} = State) ->
    {noreply, State};
handle_continue(migrate, #state{status = waiting} = State) ->
    case database:open_migrator_connection() of
        {ok, Conn} ->
            Result =
                try
                    run_migrations(Conn)
                after
                    database:close_migrator_connection(Conn)
                end,
            case Result of
                ok ->
                    logger:info("[~p] All migrations applied~n", [?SERVER]),
                    {noreply, State#state{status = done}};
                {error, Reason} ->
                    retry_later(Reason, State)
            end;
        {error, Reason} ->
            retry_later(Reason, State)
    end.

-spec retry_later(term(), state()) -> {noreply, state()}.
retry_later(Reason, State) ->
    Attempts = State#state.attempts + 1,
    Delay = retry_delay(Attempts),
    logger:warning(
        "[~p] Database unavailable (~p), retrying in ~pms (attempt ~p)~n",
        [?SERVER, Reason, Delay, Attempts]
    ),
    _ = erlang:send_after(Delay, self(), retry),
    {noreply, State#state{attempts = Attempts}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%% @end
%%--------------------------------------------------------------------
-spec handle_call(term(), gen_server:from(), state()) -> Result when
    Result :: {reply, term(), state()}.
handle_call(status, _From, State) ->
    {reply, State#state.status, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Loops back into the connection attempt on every scheduled retry.
%% @end
%%--------------------------------------------------------------------
-spec handle_info(term(), state()) -> Result when
    Result :: {noreply, state()} | {noreply, state(), {continue, migrate}}.
handle_info(retry, #state{status = waiting} = State) ->
    {noreply, State, {continue, migrate}};
handle_info({'EXIT', Pid, Reason}, State) ->
    % Exit of a linked (possibly failed) epgsql connection process,
    % already dealt with by the retry logic in handle_continue/2.
    logger:debug("[~p] Linked process ~p exited: ~p~n", [?SERVER, Pid, Reason]),
    {noreply, State};
handle_info(Info, State) ->
    logger:info("[~p] INFO: ~p~n", [?SERVER, Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate.
%% @end
%%--------------------------------------------------------------------
-spec terminate(term(), state()) -> ok.
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec retry_delay(Attempts) -> Delay when
    Attempts :: pos_integer(),
    Delay :: pos_integer().
retry_delay(Attempts) ->
    min(?BASE_RETRY_MS bsl min(Attempts - 1, 8), ?MAX_RETRY_MS).

-doc """
One full migration pass on an already-open migrator connection.

Exported so a test -- or an operator from a remote shell -- can run a
pass without going through this gen_server's boot.
""".
-spec run_migrations(Conn) -> ok | {error, term()} when
    Conn :: epgsql:connection().
run_migrations(Conn) ->
    Dir = database_queries:get_root_dir(),
    logger:info("[~p] Running migrations from ~p~n", [?SERVER, Dir]),
    %% Held across the whole pass, including migraterl's journal
    %% bootstrap. See database:lock_migrations/1 for why that bootstrap
    %% is the part that actually needs protecting.
    case database:lock_migrations(Conn) of
        ok ->
            try
                run_each(Conn, Dir, [main, repeatable, init_data, test])
            after
                database:unlock_migrations(Conn)
            end;
        {error, _} = Error ->
            Error
    end.

-doc """
Runs each namespace in order, stopping at the first that could not run.

Ordering matters -- `repeatable` assumes `main`'s tables exist -- so
this cannot become a `foreach` that keeps going. migraterl takes a
per-namespace advisory lock around planning and applying, which is what
lets several service nodes boot at once: the second blocks on the lock
and then finds nothing left to do.
""".
-spec run_each(epgsql:connection(), file:name_all(), [migration_type()]) ->
    ok | {error, term()}.
run_each(_Conn, _Dir, []) ->
    ok;
run_each(Conn, Dir, [Type | Rest]) ->
    case migrate(Conn, Dir, Type) of
        ok -> run_each(Conn, Dir, Rest);
        {error, _} = Error -> Error
    end.

-spec migrate(Conn, Dir, Type) -> ok | {error, term()} when
    Conn :: epgsql:connection(),
    Dir :: file:name_all(),
    Type :: migration_type().
migrate(Conn, Dir, init_data) ->
    Path = filename:join([Dir, "database", "migrations", "init"]),
    {ok, _} = migraterl:migrate(Conn, #{
        namespace => <<"init">>,
        sources => [{on_change, Path}]
    }),
    % Now populate the game's maps via the application pool...
    case wait_for_pool(?POOL) of
        ok ->
            MapPath = filename:join([Dir, "maps"]),
            ok = map_generator:create_map(MapPath, "Pond"),
            ok;
        {error, _} = Error ->
            Error
    end;
migrate(Conn, Dir, Type) ->
    {Suffix, Namespace, Class} =
        case Type of
            main ->
                {"main", <<"main">>, once};
            repeatable ->
                {"repeatable", <<"repeatable">>, on_change};
            test ->
                {"test", <<"test">>, on_change}
        end,
    Path = filename:join([Dir, "database", "migrations", Suffix]),
    logger:debug("[~p] MIGRATION PATH: ~p", [?SERVER, Path]),
    {ok, _} = migraterl:migrate(Conn, #{
        namespace => Namespace,
        sources => [{Class, Path}]
    }),
    ok.

-doc """
Blocks until the pgo pool actually answers a query, or gives up.

The pool's supervisor returns as soon as it is started, but its
connections are established asynchronously, so `lyceum_pool` can exist
while every query through it comes back `none_available`. The seed data
below runs seconds after boot and races exactly that -- and the race
gets likelier, not less, as more service nodes start at once.

Giving up returns an error rather than crashing so the caller folds it
into the same backoff it already uses for an unreachable database.
""".
-spec wait_for_pool(database:pool_name()) -> ok | {error, pool_unavailable}.
wait_for_pool(Pool) ->
    Deadline = erlang:monotonic_time(millisecond) + env(pool_ready_timeout_ms, ?POOL_READY_TIMEOUT_MS),
    wait_for_pool(Pool, Deadline).

-spec wait_for_pool(database:pool_name(), integer()) -> ok | {error, pool_unavailable}.
wait_for_pool(Pool, Deadline) ->
    case pool_answers(Pool) of
        true ->
            ok;
        false ->
            case erlang:monotonic_time(millisecond) < Deadline of
                true ->
                    timer:sleep(env(pool_ready_interval_ms, ?POOL_READY_INTERVAL_MS)),
                    wait_for_pool(Pool, Deadline);
                false ->
                    logger:warning("[~p] Pool ~p never became available~n", [?SERVER, Pool]),
                    {error, pool_unavailable}
            end
    end.

-spec pool_answers(database:pool_name()) -> boolean().
pool_answers(Pool) ->
    try database:query(Pool, "SELECT 1") of
        #{rows := _} -> true;
        _Other -> false
    catch
        _:_ -> false
    end.

-doc "App env with a type-checked fallback, so a bad value cannot reach arithmetic.".
-spec env(atom(), pos_integer()) -> pos_integer().
env(Key, Default) ->
    case application:get_env(world, Key, Default) of
        Value when is_integer(Value), Value > 0 -> Value;
        _Other -> Default
    end.
