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
-export([start_link/0, status/0]).
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
            try
                ok = run_migrations(Conn)
            after
                database:close_migrator_connection(Conn)
            end,
            logger:info("[~p] All migrations applied~n", [?SERVER]),
            {noreply, State#state{status = done}};
        {error, Reason} ->
            Attempts = State#state.attempts + 1,
            Delay = retry_delay(Attempts),
            logger:warning(
                "[~p] Database unavailable (~p), retrying in ~pms "
                "(attempt ~p)~n",
                [?SERVER, Reason, Delay, Attempts]
            ),
            _ = erlang:send_after(Delay, self(), retry),
            {noreply, State#state{attempts = Attempts}}
    end.

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

-spec run_migrations(Conn) -> ok when
    Conn :: epgsql:connection().
run_migrations(Conn) ->
    Dir = database_queries:get_root_dir(),
    logger:info("[~p] Running migrations from ~p~n", [?SERVER, Dir]),
    lists:foreach(
        fun(Type) -> ok = migrate(Conn, Dir, Type) end,
        [main, repeatable, init_data, test]
    ).

-spec migrate(Conn, Dir, Type) -> ok when
    Conn :: epgsql:connection(),
    Dir :: file:name_all(),
    Type :: migration_type().
migrate(Conn, Dir, init_data) ->
    Path = filename:join([Dir, "database", "migrations", "init"]),
    {ok, _} = migraterl:migrate(Conn, Path, #{repeatable => true}),
    % Now populate the game's maps via the application pool...
    MapPath = filename:join([Dir, "maps"]),
    ok = map_generator:create_map(MapPath, "Pond"),
    ok;
migrate(Conn, Dir, Type) ->
    Suffix =
        case Type of
            main ->
                "main";
            repeatable ->
                "repeatable";
            test ->
                "test"
        end,
    Path = filename:join([Dir, "database", "migrations", Suffix]),
    logger:debug("[~p] MIGRATION PATH: ~p", [?SERVER, Path]),
    Options =
        case Type of
            main ->
                #{repeatable => false};
            _ ->
                #{repeatable => true}
        end,
    {ok, _} = migraterl:migrate(Conn, Path, Options),
    ok.
