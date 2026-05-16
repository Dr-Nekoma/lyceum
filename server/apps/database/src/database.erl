-module(database).
-moduledoc """
Database wrapper around pgo, plus a one-shot epgsql connection
used exclusively at boot time by world_migrations to run migraterl
migrations (which is still hardcoded against epgsql)
""".

-export([query/2, query/3, transaction/2]).
-export([pool_configs/0, pool_options/1]).
-export([open_migrator_connection/0, close_migrator_connection/1]).

-include_lib("database/include/pool.hrl").
-export_type([pool_name/0, pool_role/0, pool_config/0, query_result/0]).

-define(PGHOST, os:getenv("PGHOST", "127.0.0.1")).
-define(PGPORT, list_to_integer(os:getenv("PGPORT", "5432"))).
-define(PGDATABASE, os:getenv("PGDATABASE", "lyceum")).

-define(PG_APP_USER, os:getenv("PGUSER", "application")).
-define(PG_APP_PASSWORD, os:getenv("PGPASSWORD", "application")).
-define(PG_AUTH_USER, os:getenv("PG_AUTH_USER", "lyceum_auth")).
-define(PG_AUTH_PASSWORD, os:getenv("PG_AUTH_PASSWORD", "lyceum_auth")).
-define(PG_MNESIA_USER, os:getenv("PG_MNESIA_USER", "mnesia")).
-define(PG_MNESIA_PASSWORD, os:getenv("PG_MNESIA_PASSWORD", "mnesia")).
-define(PG_MIGRATERL_USER, os:getenv("PG_MIGRATERL_USER", "migrations")).
-define(PG_MIGRATERL_PASSWORD, os:getenv("PG_MIGRATERL_PASSWORD", "migrations")).

%%%===================================================================
%%% Pool configuration (consumed by database_sup)
%%%===================================================================
-spec pool_configs() -> [pool_config()].
pool_configs() ->
    case application:get_env(database, pools) of
        {ok, Pools} when is_list(Pools) -> Pools;
        _ -> default_pool_configs()
    end.

-spec default_pool_configs() -> [pool_config()].
default_pool_configs() ->
    [
        #{name => lyceum_pool, role => application, size => 10},
        #{name => auth_pool, role => auth, size => 4},
        #{name => mnesia_pool, role => mnesia, size => 4}
    ].

-spec pool_options(pool_config()) -> map().
pool_options(#{role := Role, size := Size}) ->
    {User, Password} = role_credentials(Role),
    #{
        pool_size => Size,
        host => ?PGHOST,
        port => ?PGPORT,
        database => ?PGDATABASE,
        user => User,
        password => Password,
        queue_target => 50,
        queue_interval => 1000
    }.

-spec role_credentials(pool_role()) -> {string(), string()}.
role_credentials(application) -> {?PG_APP_USER, ?PG_APP_PASSWORD};
role_credentials(auth) -> {?PG_AUTH_USER, ?PG_AUTH_PASSWORD};
role_credentials(mnesia) -> {?PG_MNESIA_USER, ?PG_MNESIA_PASSWORD}.

%%%===================================================================
%%% Query API
%%%===================================================================
-spec query(pool_name(), iodata()) -> query_result().
query(Pool, SQL) ->
    query(Pool, SQL, []).

-spec query(pool_name(), iodata(), [term()]) -> query_result().
query(Pool, SQL, Params) ->
    pgo:query(
        SQL,
        [normalize_param(P) || P <- Params],
        #{
            pool => Pool,
            decode_opts => [return_rows_as_maps, column_name_as_atom]
        }
    ).

-doc """
pgo's pg_types does not auto-convert Erlang strings (lists of ints)
or atoms into text, so existing call sites pass them in those shapes. 

Normalize both into binaries at the boundary.
""".
normalize_param(P) when is_list(P) -> unicode:characters_to_binary(P);
normalize_param(P) when is_atom(P), P =/= true, P =/= false, P =/= undefined, P =/= null ->
    atom_to_binary(P, utf8);
normalize_param(P) ->
    P.

-doc """
Runs `Fun` inside a transaction on `Pool`. `pgo` binds the connection
to the calling process so any `database:query/2,3` inside `Fun`
(with the matching `Pool`) participates in the transaction.
""".
-spec transaction(pool_name(), fun(() -> term())) -> term() | {error, term()}.
transaction(Pool, Fun) ->
    pgo:transaction(Pool, Fun, #{}).

%%%===================================================================
%%% One-shot epgsql connection (boot-time migrations only)
%%%===================================================================
-doc """
Opens a short-lived epgsql connection used by migraterl inside
world_migrations. This is the ONLY remaining epgsql call site in
our application code.
""".
-spec open_migrator_connection() ->
    {ok, epgsql:connection()} | {error, epgsql:connect_error()}.
open_migrator_connection() ->
    logger:info(
        "[~p] Opening migrator connection to ~p:~p~n",
        [?MODULE, ?PGHOST, ?PGPORT]
    ),
    Config =
        #{
            host => ?PGHOST,
            port => ?PGPORT,
            username => ?PG_MIGRATERL_USER,
            password => ?PG_MIGRATERL_PASSWORD,
            database => ?PGDATABASE,
            timeout => 4000
        },
    case epgsql:connect(Config) of
        {ok, Conn} ->
            {ok, Conn};
        {error, Reason} = Err ->
            logger:error(
                "[~p] Failed to open migrator connection: ~p~n",
                [?MODULE, Reason]
            ),
            Err
    end.

-spec close_migrator_connection(epgsql:connection()) -> ok.
close_migrator_connection(Conn) ->
    _ = epgsql:close(Conn),
    ok.
