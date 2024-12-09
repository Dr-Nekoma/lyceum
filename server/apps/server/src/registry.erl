-module(registry).

-export([insert_user/2, check_user/2]).
-compile({parse_transform, do}).

-spec check_user(map(), epgsql:connection()) -> any().
check_user(#{username := Username, password := Password}, Connection) ->
    Query =
        "SELECT * FROM player.record WHERE username = $1::VARCHAR(32) "
        "AND password = $2::TEXT",
    do([postgres_m || 
	   UnprocessedUser <- {epgsql:equery(Connection, Query, [Username, Password]), select},
	   case database_utils:columns_and_rows(UnprocessedUser) of
	       [] -> fail("Could not find User");
	       [UserData | _] -> return(maps:get(e_mail, UserData))
	   end]).

-spec insert_user(map(), epgsql:connection()) -> any().
insert_user(#{username := Username,
              email := Email,
              password := Password},
            Connection) ->
    Query =
        "INSERT INTO player.record (username, e-mail, password) VALUES "
        "($1::VARCHAR(32), $2::TEXT, $3::TEXT)",
    do([postgres_m || 
	   _ <- {epgsql:equery(Connection, Query, [Username, Email, Password]), insert},
	   ok]).
