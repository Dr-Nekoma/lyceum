-module(registry_tests).

-include_lib("eunit/include/eunit.hrl").

check_user_success_test() ->
    meck:new(epgsql),
    meck:new(database_utils),
    
    meck:expect(epgsql, equery, fun(_Conn, _Query, _Params) -> 
        {ok, ["username", "e_mail", "password"], [
            [<<"user1">>, <<"user1@email.com">>, <<"1234">>]
        ]}
    end),
    
    meck:expect(database_utils, columns_and_rows, fun(Arg) ->
        logger:info("MOCK columns_and_rows called with: ~p~n", [Arg]),
        [#{username => <<"user1">>, e_mail => <<"user1@email.com">>, password => <<"1234">>}]
    end),
    
    UserMap = #{username => <<"user1">>, password => <<"1234">>},
    Result = registry:check_user(UserMap, fake_conn),
    
    ?assertEqual({ok, <<"user1@email.com">>}, Result),
    
    meck:unload(epgsql),
    meck:unload(database_utils).


insert_user_success_test() ->
    meck:new(epgsql),
    meck:new(postgres_m),

    meck:expect(epgsql, equery, fun(_Conn, _Query, _Params) ->
        {ok, 1}  
    end),

    meck:expect(postgres_m, '>>=', fun({{ok, Count}, insert}, Fun) ->
        Fun(Count)
    end),

    UserMap = #{
        username => <<"user1">>,
        email => <<"user1@email.com">>,
        password => <<"1234">>
    },

    Result = registry:insert_user(UserMap, fake_conn),
    ?assertEqual(ok, Result),

    meck:unload(epgsql),
    meck:unload(postgres_m).
    
