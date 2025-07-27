-module(registry_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%% CT Callbacks
-export([all/0, groups/0, init_per_group/2, end_per_group/2, init_per_testcase/2,
         end_per_testcase/2]).
%% Test cases
-export([test_check_user_success/1, test_insert_user_success/1]).

%%--------------------------------------------------------------------
%% CT Callbacks
%%--------------------------------------------------------------------
all() ->
    [{group, query_operations}, {group, insert_operations}].

groups() ->
    [{query_operations, [test_check_user_success]},
     {insert_operations, [test_insert_user_success]}].

init_per_group(Name, Config) ->
    ct:pal("Starting group: ~p~n", [Name]),
    Config.

end_per_group(Name, Config) ->
    ct:pal("Ending Group: ~p~n", [Name]),
    Config.

init_per_testcase(TestCase, Config) ->
    ct:comment("Ending test case: ~p", [TestCase]),

    PlayerId = <<"uid">>,
    Username = <<"user1">>,
    Email = <<"user1@email.com">>,
    Password = <<"1234">>,
    TestUser =
        #{player_id => PlayerId,
          username => Username,
          email => Email,
          password => Password},

    %% Mock database module if needed
    meck:new(epgsql),
    meck:new(database_utils),
    meck:new(postgres_m),

    [{test_user, TestUser} | Config].

end_per_testcase(TestCase, Config) ->
    ct:comment("Ending test case: ~p", [TestCase]),

    meck:unload(epgsql),
    meck:unload(database_utils),
    meck:unload(postgres_m),

    Config.

%%--------------------------------------------------------------------
%% Test Cases
%%--------------------------------------------------------------------
%% Simple Queries
test_check_user_success(Config) ->
    TestUser = ?config(test_user, Config),
    #{player_id := PlayerId,
      email := Email,
      username := Username,
      password := Password} =
        TestUser,
    Expected = {PlayerId, Email},

    meck:expect(epgsql,
                equery,
                fun(_Conn, _Query, _Params) ->
                   {ok,
                    ["uid", "username", "email", "password"],
                    [[PlayerId, Username, Email, Password]]}
                end),

    meck:expect(database_utils,
                columns_and_rows,
                fun(Arg) ->
                   ct:pal("MOCK columns_and_rows called with: ~p~n", [Arg]),
                   [TestUser]
                end),

    meck:expect(postgres_m,
                '>>=',
                fun({{ok, _, [[Uid, _, E, _]]}, select}, _) -> {ok, {Uid, E}} end),

    UserMap = #{username => <<"user1">>, password => <<"1234">>},
    Result = registry:check_user(UserMap, fake_conn),

    ?assertEqual({ok, Expected}, Result).

test_insert_user_success(Config) ->
    TestUser = ?config(test_user, Config),
    #{player_id := PlayerId,
      email := Email,
      username := Username,
      password := Password} =
        TestUser,

    meck:expect(epgsql, equery, fun(_Conn, _Query, _Params) -> {ok, 1} end),
    meck:expect(postgres_m, '>>=', fun({{ok, Count}, insert}, Fun) -> Fun(Count) end),

    UserMap =
        #{player_id => PlayerId,
          username => Username,
          email => Email,
          password => Password},

    Result = registry:insert_user(UserMap, fake_conn),
    ?assertEqual(ok, Result).
