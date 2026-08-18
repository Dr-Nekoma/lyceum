-module(player_session_SUITE).
-moduledoc """
Tests for the logic-layer session manager and the player process'
proxy-monitor cleanup.

`lyceum_service` is meck'd (the service layer has its own suite); the
real `player_session`, `player_top_level_sup` and player FSM run. A
`pg` scope is started per testcase because both `player_session`
(group `logic`) and the player FSM (`{player, Id}`) join it.
""".

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("player_state.hrl").

%% CT Callbacks
-export([all/0, init_per_testcase/2, end_per_testcase/2]).

%% Test cases
-export([
    test_login_success/1,
    test_duplicate_login_kicks_previous/1,
    test_check_user_error/1,
    test_service_error_propagates/1,
    test_no_logic_without_session_manager/1,
    test_player_cleans_up_on_proxy_death/1
]).

all() ->
    [
        test_login_success,
        test_duplicate_login_kicks_previous,
        test_check_user_error,
        test_service_error_propagates,
        test_no_logic_without_session_manager,
        test_player_cleans_up_on_proxy_death
    ].

init_per_testcase(TestCase, Config) ->
    ok = application:set_env(lyceum_cluster, node_types, [logic]),
    Scope = lyceum_cluster_test_helpers:start_scope(),
    meck:new(lyceum_service, [passthrough]),
    {ok, TopSup} = player_top_level_sup:start_link(),
    Extra =
        case TestCase of
            test_no_logic_without_session_manager ->
                [];
            _ ->
                {ok, Session} = player_session:start_link(),
                ok = lyceum_cluster_test_helpers:wait_until(
                    fun() -> lists:member(Session, lyceum_cluster:members(logic)) end
                ),
                [{session, Session}]
        end,
    [{scope, Scope}, {top_sup, TopSup} | Extra] ++ Config.

end_per_testcase(_TestCase, Config) ->
    case ?config(session, Config) of
        undefined -> ok;
        Session -> gen_server:stop(Session)
    end,
    gen_server:stop(?config(top_sup, Config)),
    _ = meck:unload(),
    ok = lyceum_cluster_test_helpers:stop_scope(?config(scope, Config)),
    ok = application:unset_env(lyceum_cluster, node_types),
    Config.

%%--------------------------------------------------------------------
%% Test Cases
%%--------------------------------------------------------------------

test_login_success(_Config) ->
    expect_check_user_ok(),
    expect_login_session(undefined),
    {ok, {PlayerPid, Email}} = player_session:login(login_request(self())),
    ?assert(is_pid(PlayerPid)),
    ?assertEqual("someone@example.com", Email),
    ?assert(is_process_alive(PlayerPid)),
    %% The player joined its pg group (the {global, Id} replacement)
    ok = lyceum_cluster_test_helpers:wait_until(
        fun() -> lists:member(PlayerPid, lyceum_cluster:members({player, 7})) end
    ).

test_duplicate_login_kicks_previous(_Config) ->
    expect_check_user_ok(),
    Previous = spawn_message_collector(),
    expect_login_session(Previous),
    {ok, {_PlayerPid, _}} = player_session:login(login_request(self())),
    receive
        {collected, Previous, {kick, _Reason}} -> ok
    after 1000 -> ct:fail(previous_session_never_kicked)
    end.

test_check_user_error(_Config) ->
    meck:expect(lyceum_service, check_user, fun(_) -> {error, "Could not find User"} end),
    ?assertEqual(
        {error, "Could not find User"},
        player_session:login(login_request(self()))
    ).

test_service_error_propagates(_Config) ->
    expect_check_user_ok(),
    meck:expect(lyceum_service, login_session, fun(_) -> {error, no_service} end),
    ?assertEqual({error, no_service}, player_session:login(login_request(self()))).

test_no_logic_without_session_manager(_Config) ->
    ?assertEqual({error, no_logic}, player_session:login(login_request(self()))).

test_player_cleans_up_on_proxy_death(_Config) ->
    expect_check_user_ok(),
    expect_login_session(undefined),
    Cleanup = ets:new(cleanup, [public]),
    meck:expect(lyceum_service, exit_map, fun(Request) ->
        true = ets:insert(Cleanup, {exit_map, Request}),
        ok
    end),
    Proxy = spawn_message_collector(),
    {ok, {PlayerPid, _}} = player_session:login(login_request(Proxy)),
    Ref = monitor(process, PlayerPid),
    exit(Proxy, kill),
    receive
        {'DOWN', Ref, process, PlayerPid, normal} -> ok
    after 1000 -> ct:fail(player_survived_proxy_death)
    end,
    %% Cleanup deactivated the character but did not delete the
    %% session row (no logout_session/logout_player call).
    ?assertMatch([{exit_map, _}], ets:lookup(Cleanup, exit_map)),
    ?assertEqual(0, meck:num_calls(lyceum_service, logout_session, '_')),
    ?assertEqual(0, meck:num_calls(lyceum_service, logout_player, '_')),
    ets:delete(Cleanup).

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

login_request(ProxyPid) ->
    #{
        username => "someone",
        password => "hunter2",
        client_pid => ProxyPid
    }.

expect_check_user_ok() ->
    meck:expect(lyceum_service, check_user, fun(#{username := _, password := _}) ->
        {ok, {7, "someone@example.com"}}
    end).

expect_login_session(PreviousPid) ->
    meck:expect(lyceum_service, login_session, fun(Cache) ->
        {ok, Cache, PreviousPid}
    end).

%% Forwards everything it receives to the test process, tagged with its
%% own pid, so kick delivery can be asserted.
spawn_message_collector() ->
    Parent = self(),
    spawn(fun Loop() ->
        receive
            Msg ->
                Parent ! {collected, self(), Msg},
                Loop()
        end
    end).
