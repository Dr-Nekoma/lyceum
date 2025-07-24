%%%-------------------------------------------------------------------
%% @doc Database module, centralizes our connection poolings & queries
%% @end
%%%-------------------------------------------------------------------

-module(database).

-export([connect/0, connect_as_dispatcher/0, connect_as_migraterl/0,
         connect_as_mnesia/0]).

-define(PGHOST, os:getenv("PGHOST", "127.0.0.1")).
-define(PGPORT, list_to_integer(os:getenv("PGPORT", "5432"))).
-define(PGUSER, os:getenv("PGUSER", "application")).
-define(PGPASSWORD, os:getenv("PGPASSWORD", "application")).
-define(PG_MIGRATERL_USER, os:getenv("PG_MIGRATERL_USER", "migrations")).
-define(PG_MIGRATERL_PASSWORD, os:getenv("PG_MIGRATERL_PASSWORD", "migrations")).
-define(PG_MNESIA_USER, os:getenv("PG_MNESIA_USER", "mnesia")).
-define(PG_MNESIA_PASSWORD, os:getenv("PG_MNESIA_PASSWORD", "mnesia")).
-define(PG_DISPATCHER_USER, os:getenv("PG_DISPATCHER_USER", "dispatcher")).
-define(PG_DISPATCHER_PASSWORD, os:getenv("PG_DISPATCHER_PASSWORD", "dispatcher")).
-define(PGDATABASE, os:getenv("PGDATABASE", "lyceum")).

-spec connect_with(User, Password) -> Result
    when User :: nonempty_string(),
         Password :: nonempty_string(),
         Error :: {error, epgsql:connect_error()},
         Ok :: {ok, epgsql:connection()},
         Result :: Ok | Error.
connect_with(User, Password) ->
    logger:info("Connecting to ~p at ~p~n", [?PGHOST, ?PGPORT]),
    Connection =
        #{host => ?PGHOST,
          port => ?PGPORT,
          username => User,
          password => Password,
          database => ?PGDATABASE,
          timeout => 4000},
    case epgsql:connect(Connection) of
        {ok, Conn} ->
            logger:info("Successfully connected to ~p~n", [?PGHOST]),
            {ok, Conn};
        {error, Reason} ->
            logger:error("Failed to connect to ~p: ~p~n", [?PGHOST, Reason]),
            {error, Reason}
    end.

-spec connect() -> {ok, epgsql:connection()} | {error, epgsql:connect_error()}.
connect() ->
    connect_with(?PGUSER, ?PGPASSWORD).

-spec connect_as_dispatcher() ->
                               {ok, epgsql:connection()} | {error, epgsql:connect_error()}.
connect_as_dispatcher() ->
    connect_with(?PG_DISPATCHER_USER, ?PG_DISPATCHER_PASSWORD).

-spec connect_as_migraterl() ->
                              {ok, epgsql:connection()} | {error, epgsql:connect_error()}.
connect_as_migraterl() ->
    connect_with(?PG_MIGRATERL_USER, ?PG_MIGRATERL_PASSWORD).

-spec connect_as_mnesia() -> {ok, epgsql:connection()} | {error, epgsql:connect_error()}.
connect_as_mnesia() ->
    connect_with(?PG_MNESIA_USER, ?PG_MNESIA_PASSWORD).
