-module(registry).

-export([insert_user/2, check_user/2]).

-compile({parse_transform, do}).

-include("player_state.hrl").

-spec check_user(Map, database:pool_name()) -> Result
    when
         Map :: #{'password':=_, 'username':=_, _=>_},
         Email :: player_email(),
         PlayerId :: player_id(),
         Reason :: string(),
         Ok :: {ok, {PlayerId, Email}},
         Error :: {error, Reason},
         Result :: Ok | Error.
check_user(#{username := Username, password := Password}, Pool) ->
    Query =
        "SELECT uid, username, password, email::TEXT "
        "FROM player.record "
        "WHERE username = $1::TEXT AND password = $2::TEXT",
    do([postgres_m
        || UnprocessedUser <- database:query(Pool, Query, [Username, Password]),
           case database_utils:columns_and_rows(UnprocessedUser) of
               [] ->
                   fail("Could not find User");
               [#{email := Email, uid := PlayerId}| _] ->
                   return({PlayerId, Email})
           end]).

-spec insert_user(map(), database:pool_name()) -> any().
insert_user(#{username := Username,
              email := Email,
              password := Password},
            Pool) ->
    Query =
        "INSERT INTO player.record (username, email, password) VALUES "
        "($1::TEXT, $2::player.email, $3::TEXT)",
    do([postgres_m
        || _ <- database:query(Pool, Query, [Username, Email, Password]), ok]).
