-module(cache_SUITE).
-moduledoc """
The session store, against a real PostgreSQL.

This is the one suite that does not mock `database`. It cannot: what is
under test is mostly what the *database* guarantees -- that a player
cannot hold two overlapping sessions, that closing one leaves it in the
history -- and a mock is only as good as its author's guess about those
guarantees.

It skips rather than fails when PostgreSQL is unreachable or the schema
has not been migrated, so `just test` still works on a machine that has
never run `just db-up`.

Every test uses its own random `player_id`, so the suite is safe to run
repeatedly against a database it does not own and safe to run beside
another copy of itself.
""".

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("player_state.hrl").

%% CT Callbacks
-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).

%% Test cases
-export([
    test_login_opens_a_session/1,
    test_login_returns_previous_client_pid/1,
    test_logout_closes_the_session/1,
    test_logout_keeps_the_history/1,
    test_get_by_id_without_session/1,
    test_close_sessions_of_node/1,
    test_stale_sessions_are_closed_once/1,
    test_concurrent_logins_leave_one_open_session/1
]).

-define(CONCURRENT_LOGINS, 12).

all() ->
    [
        test_login_opens_a_session,
        test_login_returns_previous_client_pid,
        test_logout_closes_the_session,
        test_logout_keeps_the_history,
        test_get_by_id_without_session,
        test_close_sessions_of_node,
        test_stale_sessions_are_closed_once,
        test_concurrent_logins_leave_one_open_session
    ].

init_per_suite(Config) ->
    ok = application:set_env(lyceum_cluster, node_types, [service]),
    ok = application:set_env(database, root_dir, server_root()),
    case application:ensure_all_started(database) of
        {ok, Started} ->
            case schema_present() of
                true ->
                    [{started, Started} | Config];
                false ->
                    stop_all(Started),
                    {skip, "player.session is missing; run `just db-up` first"}
            end;
        {error, Reason} ->
            {skip, lists:flatten(io_lib:format("database app unavailable: ~p", [Reason]))}
    end.

end_per_suite(Config) ->
    stop_all(?config(started, Config)),
    ok = application:unset_env(lyceum_cluster, node_types),
    ok = application:unset_env(database, root_dir),
    Config.

init_per_testcase(_TestCase, Config) ->
    [{player_id, unique_player_id()} | Config].

end_per_testcase(_TestCase, Config) ->
    purge(?config(player_id, Config)),
    Config.

%%--------------------------------------------------------------------
%% Test Cases
%%--------------------------------------------------------------------

test_login_opens_a_session(Config) ->
    PlayerId = ?config(player_id, Config),
    Cache = player(PlayerId, self()),

    %% Nobody was logged in, so there is no previous session to kick.
    ?assertEqual({ok, Cache, undefined}, cache:login(Cache, node())),

    {ok, Stored} = cache:get_by_id(PlayerId),
    ?assertEqual(PlayerId, Stored#player_cache.player_id),
    ?assertEqual(Cache#player_cache.username, Stored#player_cache.username),
    ?assertEqual(Cache#player_cache.email, Stored#player_cache.email),
    %% The pid survives the round trip through bytea, which text
    %% encoding would not have managed across nodes.
    ?assertEqual(self(), Stored#player_cache.client_pid),
    ?assertEqual(1, open_sessions(PlayerId)).

test_login_returns_previous_client_pid(Config) ->
    PlayerId = ?config(player_id, Config),
    First = spawn(fun() -> receive stop -> ok end end),

    {ok, _, undefined} = cache:login(player(PlayerId, First), node()),

    %% The second login is handed the first session's client pid, which
    %% is the whole mechanism behind kicking a duplicate login.
    Second = player(PlayerId, self()),
    ?assertEqual({ok, Second, First}, cache:login(Second, node())),

    %% ... and it replaced it rather than joining it.
    ?assertEqual(1, open_sessions(PlayerId)),
    ?assertEqual(2, total_sessions(PlayerId)),
    First ! stop.

test_logout_closes_the_session(Config) ->
    PlayerId = ?config(player_id, Config),
    {ok, _, undefined} = cache:login(player(PlayerId, self()), node()),

    ?assertEqual(ok, cache:logout(PlayerId)),
    ?assertEqual({error, not_found}, cache:get_by_id(PlayerId)),
    ?assertEqual(0, open_sessions(PlayerId)),

    %% Logging out twice is not an error: the second finds nothing open.
    ?assertEqual(ok, cache:logout(PlayerId)).

test_logout_keeps_the_history(Config) ->
    PlayerId = ?config(player_id, Config),
    {ok, _, undefined} = cache:login(player(PlayerId, self()), node()),
    ok = cache:logout(PlayerId),

    %% Closed, not deleted: the row is the record that this player was
    %% online, and when.
    ?assertEqual(1, total_sessions(PlayerId)),
    ?assertEqual(0, open_sessions(PlayerId)),
    ?assertEqual(0, empty_periods(PlayerId)).

test_get_by_id_without_session(Config) ->
    ?assertEqual({error, not_found}, cache:get_by_id(?config(player_id, Config))).

test_close_sessions_of_node(Config) ->
    PlayerId = ?config(player_id, Config),
    Owner = 'lyceum_logic@nowhere',
    {ok, _, undefined} = cache:login(player(PlayerId, self()), Owner),
    ?assertEqual(1, open_sessions(PlayerId)),

    %% What the reaper does when a logic node dies: its players are gone
    %% with it, so their sessions are over.
    ?assertEqual(ok, cache:close_sessions_of_node(Owner)),
    ?assertEqual(0, open_sessions(PlayerId)),
    ?assertEqual(1, total_sessions(PlayerId)).

test_stale_sessions_are_closed_once(Config) ->
    PlayerId = ?config(player_id, Config),
    Owner = 'lyceum_logic@nowhere',
    {ok, _, undefined} = cache:login(player(PlayerId, self()), Owner),

    %% A session opened a moment ago is not stale, whatever its owner.
    {ok, Fresh} = cache:stale_sessions(3600, 100),
    ?assertNot(lists:member({PlayerId, Owner}, Fresh)),

    %% With the threshold at zero it is a candidate...
    {ok, Candidates} = cache:stale_sessions(0, 100),
    ?assert(lists:member({PlayerId, Owner}, Candidates)),

    %% ... and closing it reports the one row it actually changed.
    ?assertEqual({ok, 1}, cache:close_stale_session(PlayerId, Owner, 0)),
    ?assertEqual(0, open_sessions(PlayerId)),

    %% Repeating it closes nothing: the re-check in the statement is
    %% what keeps a session that stopped being stale from being reaped.
    ?assertEqual({ok, 0}, cache:close_stale_session(PlayerId, Owner, 0)).

test_concurrent_logins_leave_one_open_session(Config) ->
    PlayerId = ?config(player_id, Config),
    Parent = self(),

    %% Every worker logs the same player in at the same moment. The
    %% advisory lock serialises them and the temporal primary key is the
    %% backstop, so the outcome must be exactly one live session no
    %% matter how the interleaving falls.
    Workers = [
        spawn_monitor(fun() ->
            Client = spawn(fun() -> receive stop -> ok end end),
            Parent ! {result, self(), cache:login(player(PlayerId, Client), node())}
        end)
     || _ <- lists:seq(1, ?CONCURRENT_LOGINS)
    ],
    Results = collect(Workers, []),

    Succeeded = [R || {ok, _, _} = R <- Results],
    Failed = Results -- Succeeded,

    %% A crashed worker must fail the test rather than be absorbed by a
    %% count that only looks at the survivors.
    ?assertEqual(?CONCURRENT_LOGINS, length(Results)),
    ?assertEqual([], Failed),
    ?assertEqual(?CONCURRENT_LOGINS, length(Succeeded)),

    ?assertEqual(1, open_sessions(PlayerId)),
    ?assertEqual(?CONCURRENT_LOGINS, total_sessions(PlayerId)),
    %% Adjacent periods, never overlapping and never degenerate.
    ?assertEqual(0, empty_periods(PlayerId)),

    %% Exactly one login found no predecessor. The other eleven each
    %% picked up the session before them, which is the chain that makes
    %% every duplicate login kick somebody.
    Firsts = [R || {ok, _, undefined} = R <- Succeeded],
    ?assertEqual(1, length(Firsts)).

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

collect([], Acc) ->
    Acc;
collect([{Pid, Ref} | Rest], Acc) ->
    receive
        {result, Pid, Result} ->
            %% Drain the DOWN so it cannot be mistaken for another
            %% worker's later.
            receive
                {'DOWN', Ref, process, Pid, _} -> ok
            after 5000 -> ct:fail({worker_never_exited, Pid})
            end,
            collect(Rest, [Result | Acc]);
        {'DOWN', Ref, process, Pid, Reason} when Reason =/= normal ->
            ct:fail({worker_crashed, Pid, Reason})
    after 15000 ->
        ct:fail({worker_timeout, Pid})
    end.

player(PlayerId, ClientPid) ->
    #player_cache{
        player_id = PlayerId,
        username = "session_" ++ integer_to_list(PlayerId),
        email = "session_" ++ integer_to_list(PlayerId) ++ "@example.com",
        client_pid = ClientPid
    }.

-doc """
A player id no other run will pick.

`erlang:unique_integer/1` restarts with the VM, so two `rebar3 ct` runs
against the same database would hand out the same ids and the second
would be testing the first run's leftovers.
""".
unique_player_id() ->
    <<Id:63/unsigned-integer, _:1>> = crypto:strong_rand_bytes(8),
    Id.

open_sessions(PlayerId) ->
    count("SELECT count(*) AS n FROM player.session "
          "WHERE player_id = $1 AND upper(valid_at) = 'infinity'", PlayerId).

total_sessions(PlayerId) ->
    count("SELECT count(*) AS n FROM player.session WHERE player_id = $1", PlayerId).

empty_periods(PlayerId) ->
    count("SELECT count(*) AS n FROM player.session "
          "WHERE player_id = $1 AND isempty(valid_at)", PlayerId).

count(SQL, PlayerId) ->
    #{rows := [#{n := N}]} = database:query(lyceum_pool, SQL, [PlayerId]),
    N.

purge(PlayerId) ->
    _ = database:query(lyceum_pool, "DELETE FROM player.session WHERE player_id = $1", [PlayerId]),
    ok.

-doc "True only if the database answers *and* has been migrated.".
schema_present() ->
    Query = "SELECT to_regclass('player.session') IS NOT NULL AS present",
    try database:query(lyceum_pool, Query, []) of
        #{rows := [#{present := true}]} -> true;
        _Other -> false
    catch
        _:_ -> false
    end.

-doc """
The server directory, where `database/queries` lives.

Under CT the code path is `<server>/_build/test/lib/database`, which is
not a release layout, so `database_queries` has to be pointed at the
tree explicitly -- the same reason the `just cluster` nodes set it.
""".
server_root() ->
    lists:foldl(
        fun(_, Path) -> filename:dirname(Path) end,
        filename:absname(code:lib_dir(database)),
        lists:seq(1, 4)
    ).

stop_all(undefined) ->
    ok;
stop_all(Started) ->
    _ = [application:stop(App) || App <- lists:reverse(Started)],
    ok.
