-module(user_handler).

-export([insert_user/1]).

-record(user_registry, {username = none, password = none, email = none}).

insert_user(#user_registry{username = Username, password = Password, email = Email}, Connection) ->
    
    {ok, 1} = epgsql:squery(Connection, "INSERT INTO \"user\" VALUES (" ++  Username++ ", " ++ Password ++ ", " ++ Email ++ ")"),
    ok = epgsql:close(Connection).

insert_user(Registry) ->
    {ok, Connection} =
	epgsql:connect(#{host => "localhost",
			 username => "admin",
			 password => "admin",
			 database => "mmo",
			 timeout => 4000
			}),
    insert_user(Registry, Connection).

