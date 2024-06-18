-module(character).

-export([create/2]).

create(#{name := Name, username := Username, email := Email}, Connection) ->
    Query = "INSERT INTO \"character\" (\"name\",  \"e-mail\", \"username\") VALUES ($1::VARCHAR(18), $2::TEXT, $1::VARCHAR(32))",
    {ok, _, _} = epgsql:equery(Connection, Query, [Name, Username, Email]).

%% create_stats(#{name := Name, username := Username, email := Email}, Connection) ->
%%     Query = "INSERT INTO \"character\" (\"name\",  \"e-mail\", \"username\") VALUES ($1::VARCHAR(18), $2::TEXT, $1::VARCHAR(32))",
%%     {ok, _, _} = epgsql:equery(Connection, Query, [Name, Username, Email]).

