%%%-------------------------------------------------------------------
%% @doc Database module, centralizes our connection poolings, queries
%%      and migrations.
%% @end
%%%-------------------------------------------------------------------

-module(database).

-export([connect/0]).

-define(PGHOST, os:getenv("PGHOST", "127.0.0.1")).
-define(PGPORT, os:getenv("PGPORT", 5432)).
-define(PGUSER, os:getenv("PGUSER", "admin")).
-define(PGPASSWORD, os:getenv("PGPASSWORD", "admin")).
-define(PGDATABASE, os:getenv("PGDATABASE", "mmo")).

connect() ->
    io:format("Connecting to ~p at ~p~n", [?PGHOST, ?PGPORT]),
    Connection =
        #{host => ?PGHOST,
          username => ?PGUSER,
          password => ?PGPASSWORD,
          database => ?PGDATABASE,
          timeout => 4000},
    case epgsql:connect(Connection) of
        {ok, Conn} ->
            io:format("Successfully connected to ~p~n", [?PGHOST]),
            {ok, Conn};
        {error, Reason} ->
            io:format("Failed to connect to ~p: ~p~n", [?PGHOST, Reason]),
            {error, Reason}
    end.
