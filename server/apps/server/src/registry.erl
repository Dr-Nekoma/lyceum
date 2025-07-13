-module(registry).

-export([insert_user/2, check_user/2]).

-compile({parse_transform, do}).

%% TODO: fix this type
-dialyzer({nowarn_function, [check_user/2]}).

-spec check_user(Map, epgsql:connection()) -> Result
    when 
         Map :: #{'password':=_, 'username':=_, _=>_},
         Email :: string(),
         Message :: string(),
         Result :: {ok, Email} | {ok, any(), any()} | {ok, any(), any(), any()} | {error, Message}.
check_user(#{username := Username, password := Password}, Connection) ->
    Query =
        "SELECT uid, username, password, email::TEXT "
        "FROM player.record "
        "WHERE username = $1::TEXT AND password = $2::TEXT",
    do([postgres_m
        || UnprocessedUser <- {epgsql:equery(Connection, Query, [Username, Password]), select},
           case database_utils:columns_and_rows(UnprocessedUser) of
               [] ->
                   fail("Could not find User");
               [#{email := Email, uid := PlayerId}| _] ->
                   return({PlayerId, Email})
           end]).

-spec insert_user(map(), epgsql:connection()) -> any().
insert_user(#{username := Username,
              email := Email,
              password := Password},
            Connection) ->
    Query =
        "INSERT INTO player.record (username, email, password) VALUES "
        "($1::TEXT, $2::player.email, $3::TEXT)",
    do([postgres_m
        || _ <- {epgsql:equery(Connection, Query, [Username, Email, Password]), insert}, ok]).
