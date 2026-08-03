-module(character_active_SUITE).
-moduledoc """
Presence in the world, against a real PostgreSQL.

`character.active` became a temporal table, so activating and
deactivating changed from inserting and deleting a row to opening and
closing a period. Everything above the SQL still calls
`character:activate/2` and `character:deactivate/4` and cannot tell the
difference -- which is exactly why this needs testing at the SQL level
rather than through a mock.

Two things are at stake. The visible one is that "who is in the world"
still answers the same; the easily-missed one is that the join in
`select_nearby_characters.sql` must look only at open periods, since a
character who has played twice now has two rows and a careless join
would show them twice.

Uses the seeded characters and skips without them, so it needs
`just db-reset` rather than a fixture of its own.
""".

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%% CT Callbacks
-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).

%% Test cases
-export([
    test_activate_opens_a_period/1,
    test_activate_twice_is_idempotent/1,
    test_deactivate_closes_and_keeps_history/1,
    test_reactivating_adds_a_second_period/1,
    test_nearby_sees_only_active_characters/1
]).

-define(POOL, lyceum_pool).
-define(CHARACTER, "Huneric").
-define(USERNAME, "mmagueta").
-define(EMAIL, "mmagueta@example.com").
-define(OTHER, "Scipio").
-define(OTHER_USERNAME, "benin").
-define(OTHER_EMAIL, "benin@example.com").
-define(MAP, "Pond").

all() ->
    [
        test_activate_opens_a_period,
        test_activate_twice_is_idempotent,
        test_deactivate_closes_and_keeps_history,
        test_reactivating_adds_a_second_period,
        test_nearby_sees_only_active_characters
    ].

init_per_suite(Config) ->
    ok = application:set_env(lyceum_cluster, node_types, [service]),
    ok = application:set_env(database, root_dir, server_root()),
    case application:ensure_all_started(database) of
        {ok, Started} ->
            case seeded() of
                true ->
                    [{started, Started} | Config];
                false ->
                    stop_all(Started),
                    {skip, "seed characters are missing; run `just db-reset` first"}
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
    purge(),
    Config.

end_per_testcase(_TestCase, Config) ->
    purge(),
    Config.

%%--------------------------------------------------------------------
%% Test Cases
%%--------------------------------------------------------------------

test_activate_opens_a_period(_Config) ->
    ?assertEqual(ok, activate(?CHARACTER, ?EMAIL, ?USERNAME)),
    ?assertEqual(1, open_periods(?CHARACTER)),
    ?assertEqual(1, total_periods(?CHARACTER)).

test_activate_twice_is_idempotent(_Config) ->
    ok = activate(?CHARACTER, ?EMAIL, ?USERNAME),
    ok = activate(?CHARACTER, ?EMAIL, ?USERNAME),

    %% Joining a map twice without leaving used to be absorbed by
    %% ON CONFLICT DO NOTHING. It must still not open a second period,
    %% or the character would be in the world twice over.
    ?assertEqual(1, open_periods(?CHARACTER)),
    ?assertEqual(1, total_periods(?CHARACTER)).

test_deactivate_closes_and_keeps_history(_Config) ->
    ok = activate(?CHARACTER, ?EMAIL, ?USERNAME),
    ?assertEqual(ok, deactivate(?CHARACTER, ?EMAIL, ?USERNAME)),

    %% Closed, not deleted: how long the character was in the world is
    %% the thing the old DELETE threw away.
    ?assertEqual(0, open_periods(?CHARACTER)),
    ?assertEqual(1, total_periods(?CHARACTER)),
    ?assertEqual(0, empty_periods(?CHARACTER)),

    %% Leaving twice is not an error; the second finds nothing open.
    ?assertEqual(ok, deactivate(?CHARACTER, ?EMAIL, ?USERNAME)),
    ?assertEqual(1, total_periods(?CHARACTER)).

test_reactivating_adds_a_second_period(_Config) ->
    ok = activate(?CHARACTER, ?EMAIL, ?USERNAME),
    ok = deactivate(?CHARACTER, ?EMAIL, ?USERNAME),
    ok = activate(?CHARACTER, ?EMAIL, ?USERNAME),

    ?assertEqual(2, total_periods(?CHARACTER)),
    ?assertEqual(1, open_periods(?CHARACTER)),
    %% Adjacent, never overlapping: the temporal primary key would have
    %% refused the second period otherwise.
    ?assertEqual(0, empty_periods(?CHARACTER)).

test_nearby_sees_only_active_characters(_Config) ->
    ok = activate(?CHARACTER, ?EMAIL, ?USERNAME),
    ok = activate(?OTHER, ?OTHER_EMAIL, ?OTHER_USERNAME),
    ?assert(lists:member(?OTHER, nearby(?CHARACTER))),

    %% The one who left is gone from the answer...
    ok = deactivate(?OTHER, ?OTHER_EMAIL, ?OTHER_USERNAME),
    ?assertNot(lists:member(?OTHER, nearby(?CHARACTER))),

    %% ... and coming back lists them once, not once per period. This is
    %% what the explicit join on the open period buys: a natural join
    %% over the whole history would return a row per visit.
    ok = activate(?OTHER, ?OTHER_EMAIL, ?OTHER_USERNAME),
    Nearby = nearby(?CHARACTER),
    ?assertEqual(1, length([N || N <- Nearby, N =:= ?OTHER])).

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

activate(Name, Email, Username) ->
    character:activate(#{name => Name, email => Email, username => Username}, ?POOL).

deactivate(Name, Email, Username) ->
    character:deactivate(Name, Email, Username, ?POOL).

nearby(Name) ->
    Request = #{
        name => Name,
        username => ?USERNAME,
        email => ?EMAIL,
        map_name => ?MAP
    },
    {ok, Characters} = character:retrieve_near_players(Request, ?POOL),
    [maps:get(name, C) || C <- Characters].

open_periods(Name) ->
    count(
        "SELECT count(*) AS n FROM character.active "
        "WHERE name = $1 AND upper(valid_at) = 'infinity'",
        Name
    ).

total_periods(Name) ->
    count("SELECT count(*) AS n FROM character.active WHERE name = $1", Name).

empty_periods(Name) ->
    count(
        "SELECT count(*) AS n FROM character.active "
        "WHERE name = $1 AND isempty(valid_at)",
        Name
    ).

count(SQL, Name) ->
    #{rows := [#{n := N}]} = database:query(?POOL, SQL, [Name]),
    N.

purge() ->
    _ = database:query(?POOL, "DELETE FROM character.active", []),
    ok.

seeded() ->
    Query =
        "SELECT count(*) AS n FROM character.view "
        "WHERE name = $1 AND map_name = $2",
    try database:query(?POOL, Query, [?CHARACTER, ?MAP]) of
        #{rows := [#{n := N}]} when N > 0 -> true;
        _Other -> false
    catch
        _:_ -> false
    end.

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
