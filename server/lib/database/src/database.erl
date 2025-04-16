%%%-------------------------------------------------------------------
%% @doc Database module, centralizes our connection poolings, queries
%%      and migrations.
%% @end
%%%-------------------------------------------------------------------

-module(database).

-export([connect/0]).

-define(PGHOST, os:getenv("PGHOST", "127.0.0.1")).
-define(PGPORT, list_to_integer(os:getenv("PGPORT", "5432"))).
-define(PGUSER, os:getenv("PGUSER", "admin")).
-define(PGPASSWORD, os:getenv("PGPASSWORD", "admin")).
-define(PGDATABASE, os:getenv("PGDATABASE", "mmo")).

-spec connect() -> {ok, epgsql:connection()} | {error, epgsql:connect_error()}.
connect() ->
    logger:info("Connecting to ~p at ~p~n", [?PGHOST, ?PGPORT]),
    Connection =
        #{host => ?PGHOST,
          port => ?PGPORT,
          username => ?PGUSER,
          password => ?PGPASSWORD,
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
