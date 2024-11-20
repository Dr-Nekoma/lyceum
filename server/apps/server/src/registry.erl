-module(registry).

-export([insert_user/2, check_user/2]).

check_user(#{username := Username, password := Password}, Connection) ->
    Query =
        "SELECT * FROM player.record WHERE username = $1::VARCHAR(32) "
        "AND password = $2::TEXT",
    Result = epgsql:equery(Connection, Query, [Username, Password]),
    Fun = fun(FullColumns, Values) ->
             case database_utils:columns_and_rows(FullColumns, Values) of
                 [] -> {error, "Could not find User"};
                 [UserData | _] -> {ok, maps:get(e_mail, UserData)}
             end
          end,
    database_utils:process_postgres_result(Result, select, Fun).

insert_user(#{username := Username,
              email := Email,
              password := Password},
            Connection) ->
    Query =
        "INSERT INTO player.record (username, e-mail, password) VALUES "
        "($1::VARCHAR(32), $2::TEXT, $3::TEXT)",
    Result = epgsql:equery(Connection, Query, [Username, Email, Password]),
    Fun = fun(_, _, _) -> ok end,
    database_utils:process_postgres_result(Result, insert, Fun).
