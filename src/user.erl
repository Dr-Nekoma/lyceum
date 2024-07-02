-module(user).

-export([insert_user/2, check_user/2]).

check_user(#{username := Username, password := Password}, Connection) ->
    Query = "SELECT * FROM user WHERE username = $1::VARCHAR(32) AND password = $2::TEXT",
    {ok, _, _} = epgsql:equery(Connection, Query, [Username, Password]).

insert_user(#{username := Username, email := Email, password := Password}, Connection) ->
    Query = "INSERT INTO user  (username, e-mail, password) VALUES ($1::VARCHAR(32), $2::TEXT, $3::TEXT)",
    {ok, _, _} = epgsql:equery(Connection, Query, [Username, Email, Password]).
