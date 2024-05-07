-module(user_handler).

-export([insert_user/2, database_connect/0]).
-include("user_registry.hrl").

insert_user(Record, Connection) ->
    Query = "INSERT INTO \"user\" (\"username\", \"e-mail\", \"password\") VALUES ('" ++  Record#user_registry.username ++ "','" ++ Record#user_registry.email ++ "','" ++ Record#user_registry.password ++ "')",
    {ok, 1} = epgsql:squery(Connection, Query),
    ok = epgsql:close(Connection).

database_connect() ->
    {ok, Connection} =
	epgsql:connect(#{host => "localhost",
			 username => "admin",
			 password => "admin",
			 database => "mmo",
			 timeout => 4000
			}),
    Connection.

