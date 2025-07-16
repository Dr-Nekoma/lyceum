-module(storage_mnesia_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%% Include the record definitions from your project
-include("player_state.hrl").

%% CT Callbacks
-export([all/0, groups/0, init_per_group/2, end_per_group/2, init_per_testcase/2,
         end_per_testcase/2]).
%% Test cases
-export([test_get_by_id_success/1, test_get_by_username_success/1,
         test_get_by_email_success/1]).
-export([test_login_success/1, test_logout_success/1, test_upsert_record_success/1]).
-export([test_get_by_id_failure/1, test_get_by_username_failure/1,
         test_get_by_email_failure/1]).

%%--------------------------------------------------------------------
%% CT Callbacks
%%--------------------------------------------------------------------

all() ->
    [{group, query_operations}, {group, cache_functions}].

groups() ->
    [{query_operations,
      [test_get_by_id_success,
       test_get_by_id_failure,
       test_get_by_username_success,
       test_get_by_username_failure,
       test_get_by_email_success,
       test_get_by_email_failure]},
     {cache_functions, [test_login_success, test_logout_success, test_upsert_record_success]}].

init_per_group(Name, Config) ->
    ct:pal("Starting group: ~p~n", [Name]),
    Config.

end_per_group(Name, Config) ->
    ct:pal("Ending Group: ~p~n", [Name]),
    Config.

init_per_testcase(TestCase, Config) ->
    %% Mock database module if needed
    meck:new(epgsql),
    meck:new(database),
    meck:expect(database, connect_as_mnesia, fun() -> {ok, fake_connection} end),

    {ok, Pid} = storage_mnesia:start_link(),
    ct:pal("[~p] Gen Server started at ~p", [TestCase, Pid]),

    %% Create test data
    TestPlayer =
        #player_cache{player_id = "uid",
                      username = "player",
                      email = "player@email.com",
                      client_pid = self()},

    gen_server:cast(storage_mnesia, {upsert_record, TestPlayer}),
    timer:sleep(100), %% Allow cast to complete

    [{server_pid, Pid}, {test_player_01, TestPlayer} | Config].

end_per_testcase(TestCase, Config) ->
    ct:comment("Ending test case: ~p", [TestCase]),

    %% Stop the server
    ServerPid = ?config(server_pid, Config),
    gen_server:stop(ServerPid),

    %% Clean up test tables
    mnesia:stop(),
    mnesia:delete_schema([node()]),
    catch mnesia:delete_table(player_cache),

    meck:unload(database),
    meck:unload(epgsql),

    Config.

%%--------------------------------------------------------------------
%% Test Cases
%%--------------------------------------------------------------------
%% Simple Queries
test_get_by_id_success(Config) ->
    TestCache = ?config(test_player_01, Config),
    PlayerId = TestCache#player_cache.player_id,
    Username = TestCache#player_cache.username,
    Email = TestCache#player_cache.email,

    %% Test get by ID
    {ok, C} = gen_server:call(storage_mnesia, {get_by_id, PlayerId}),

    ?assertEqual(PlayerId, C#player_cache.player_id),
    ?assertEqual(Username, C#player_cache.username),
    ?assertEqual(Email, C#player_cache.email).

test_get_by_id_failure(_Config) ->
    {error, _} = gen_server:call(storage_mnesia, {get_by_id, "nonexistent"}).

test_get_by_username_success(Config) ->
    TestCache = ?config(test_player_01, Config),
    PlayerId = TestCache#player_cache.player_id,
    Username = TestCache#player_cache.username,
    Email = TestCache#player_cache.email,

    %% Test get by ID
    {ok, C} = gen_server:call(storage_mnesia, {get_by_username, Username}),

    ?assertEqual(PlayerId, C#player_cache.player_id),
    ?assertEqual(Username, C#player_cache.username),
    ?assertEqual(Email, C#player_cache.email).

test_get_by_username_failure(_Config) ->
    {error, _} = gen_server:call(storage_mnesia, {get_by_username, "nonexistent"}).

test_get_by_email_success(Config) ->
    TestCache = ?config(test_player_01, Config),
    PlayerId = TestCache#player_cache.player_id,
    Username = TestCache#player_cache.username,
    Email = TestCache#player_cache.email,

    %% Test get by ID
    {ok, C} = gen_server:call(storage_mnesia, {get_by_email, Email}),

    ?assertEqual(PlayerId, C#player_cache.player_id),
    ?assertEqual(Username, C#player_cache.username),
    ?assertEqual(Email, C#player_cache.email).

test_get_by_email_failure(_Config) ->
    {error, _} = gen_server:call(storage_mnesia, {get_by_email, "nonexistent"}).

%% Cache Operations
test_login_success(Config) ->
    NewPlayer =
        #player_cache{player_id = "uid2",
                      username = "mmagueta",
                      email = "mmagueta@example.com",
                      client_pid = self()},
    Request1 = {login, NewPlayer},

    %% Test if a new player is properly cached
    {ok, C1} = gen_server:call(storage_mnesia, Request1),

    ?assertEqual(NewPlayer#player_cache.player_id, C1#player_cache.player_id),
    ?assertEqual(NewPlayer#player_cache.username, C1#player_cache.username),
    ?assertEqual(NewPlayer#player_cache.email, C1#player_cache.email),

    %% Now the same for a player already logged in
    TestCache = ?config(test_player_01, Config),
    Request2 = {login, TestCache},
    {ok, C2} = gen_server:call(storage_mnesia, Request2),

    ?assertEqual(TestCache#player_cache.player_id, C2#player_cache.player_id),
    ?assertEqual(TestCache#player_cache.username, C2#player_cache.username),
    ?assertEqual(TestCache#player_cache.email, C2#player_cache.email).

test_upsert_record_success(_Config) ->
    NewPlayer =
        #player_cache{player_id = "uid2",
                      username = "mmagueta",
                      email = "mmagueta@example.com",
                      client_pid = self()},
    Request1 = {upsert_record, NewPlayer},

    %% Test if a new player is properly cached
    ok = gen_server:cast(storage_mnesia, Request1),
    %% Allow cast to complete
    timer:sleep(200),

    Request2 = {get_by_id, NewPlayer#player_cache.player_id},
    {ok, C1} = gen_server:call(storage_mnesia, Request2),

    ?assertEqual(NewPlayer#player_cache.player_id, C1#player_cache.player_id),
    ?assertEqual(NewPlayer#player_cache.username, C1#player_cache.username),
    ?assertEqual(NewPlayer#player_cache.email, C1#player_cache.email).

test_logout_success(Config) ->
    TestPlayer = ?config(test_player_01, Config),
    Request1 = {logout, TestPlayer#player_cache.player_id},
    ok = gen_server:cast(storage_mnesia, Request1),
    %% Allow cast to complete
    timer:sleep(200),

    Request2 = {get_by_id, TestPlayer#player_cache.player_id},
    {error, _} = gen_server:call(storage_mnesia, Request2).

%%--------------------------------------------------------------------
%% Helper Functions
%%--------------------------------------------------------------------
