-module(client_proxy_SUITE).
-moduledoc """
Tests for the per-session client proxy.

The test process plays the Zig client (it is the `client_pid`), a
spawned stub plays the player process, and `player_session` is meck'd
so no logic/service layer is needed.
""".

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%% CT Callbacks
-export([all/0, init_per_testcase/2, end_per_testcase/2]).

%% Test cases
-export([
    test_login_success_and_relay/1,
    test_login_failure/1,
    test_login_error_atom_is_formatted/1,
    test_kick/1,
    test_player_down_abnormal/1,
    test_player_down_normal/1
]).

all() ->
    [
        test_login_success_and_relay,
        test_login_failure,
        test_login_error_atom_is_formatted,
        test_kick,
        test_player_down_abnormal,
        test_player_down_normal
    ].

init_per_testcase(_TestCase, Config) ->
    meck:new(player_session, [passthrough]),
    Config.

end_per_testcase(_TestCase, Config) ->
    _ = meck:unload(),
    Config.

%%--------------------------------------------------------------------
%% Test Cases
%%--------------------------------------------------------------------

test_login_success_and_relay(_Config) ->
    Player = start_fake_player(),
    expect_login_ok(Player),
    {ok, Proxy} = start_proxy(),

    %% The client receives the proxy pid as its handler, not the
    %% player pid: the handler must live on the frontend node.
    ProxyPid =
        receive
            {ok, {Pid, "someone@example.com"}} -> Pid
        after 1000 -> ct:fail(no_login_reply)
        end,
    ?assertEqual(Proxy, ProxyPid),

    %% client -> player relay is verbatim
    Proxy ! {list_characters, #{username => "someone"}},
    receive
        {player_got, {list_characters, #{username := "someone"}}} -> ok
    after 1000 -> ct:fail(player_never_got_message)
    end,

    %% player -> client relay strips the {reply, _} tag
    Proxy ! {reply, {ok, [a_character]}},
    receive
        {ok, [a_character]} -> ok
    after 1000 -> ct:fail(client_never_got_reply)
    end,

    stop_fake_player(Player).

test_login_failure(_Config) ->
    meck:expect(player_session, login, fun(_) -> {error, "Could not find User"} end),
    {ok, Proxy} = start_proxy(),
    Ref = monitor(process, Proxy),
    receive
        {error, "Could not find User"} -> ok
    after 1000 -> ct:fail(no_error_reply)
    end,
    receive
        {'DOWN', Ref, process, Proxy, normal} -> ok
    after 1000 -> ct:fail(proxy_did_not_stop)
    end.

test_login_error_atom_is_formatted(_Config) ->
    meck:expect(player_session, login, fun(_) -> {error, no_logic} end),
    {ok, _Proxy} = start_proxy(),
    receive
        {error, "No game server available"} -> ok
    after 1000 -> ct:fail(no_error_reply)
    end.

test_kick(_Config) ->
    Player = start_fake_player(),
    expect_login_ok(Player),
    {ok, Proxy} = start_proxy(),
    receive
        {ok, {Proxy, _}} -> ok
    after 1000 -> ct:fail(no_login_reply)
    end,
    Ref = monitor(process, Proxy),
    Proxy ! {kick, "Logged in from another client"},
    receive
        {error, "Logged in from another client"} -> ok
    after 1000 -> ct:fail(no_kick_reply)
    end,
    receive
        {'DOWN', Ref, process, Proxy, normal} -> ok
    after 1000 -> ct:fail(proxy_did_not_stop)
    end,
    stop_fake_player(Player).

test_player_down_abnormal(_Config) ->
    Player = start_fake_player(),
    expect_login_ok(Player),
    {ok, Proxy} = start_proxy(),
    receive
        {ok, {Proxy, _}} -> ok
    after 1000 -> ct:fail(no_login_reply)
    end,
    Ref = monitor(process, Proxy),
    exit(Player, kill),
    receive
        {error, "Session lost"} -> ok
    after 1000 -> ct:fail(no_session_lost_reply)
    end,
    receive
        {'DOWN', Ref, process, Proxy, normal} -> ok
    after 1000 -> ct:fail(proxy_did_not_stop)
    end.

test_player_down_normal(_Config) ->
    Player = start_fake_player(),
    expect_login_ok(Player),
    {ok, Proxy} = start_proxy(),
    receive
        {ok, {Proxy, _}} -> ok
    after 1000 -> ct:fail(no_login_reply)
    end,
    Ref = monitor(process, Proxy),
    stop_fake_player(Player),
    %% A normal player exit (logout) stops the proxy with no noise
    %% towards the client.
    receive
        {'DOWN', Ref, process, Proxy, normal} -> ok
    after 1000 -> ct:fail(proxy_did_not_stop)
    end,
    receive
        Unexpected -> ct:fail({unexpected_client_message, Unexpected})
    after 100 -> ok
    end.

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

start_proxy() ->
    client_proxy:start_link(#{
        client_pid => self(),
        request => #{username => "someone", password => "hunter2"}
    }).

expect_login_ok(PlayerPid) ->
    meck:expect(player_session, login, fun(#{client_pid := _}) ->
        {ok, {PlayerPid, "someone@example.com"}}
    end).

%% A stand-in for the player FSM: forwards whatever it receives to the
%% test process, tagged, so relays can be asserted.
start_fake_player() ->
    Parent = self(),
    spawn(fun() -> fake_player_loop(Parent) end).

fake_player_loop(Parent) ->
    receive
        stop ->
            ok;
        Msg ->
            Parent ! {player_got, Msg},
            fake_player_loop(Parent)
    end.

stop_fake_player(Player) ->
    Ref = monitor(process, Player),
    Player ! stop,
    receive
        {'DOWN', Ref, process, Player, _} -> ok
    after 1000 -> error(fake_player_did_not_stop)
    end.
