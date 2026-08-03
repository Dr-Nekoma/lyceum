-module(world_migrations_SUITE).
-moduledoc """
Several service nodes may boot at once, so several migration passes may
run at once. This is the suite that says that is safe.

Nothing in this repository implements that safety: migraterl holds a
per-namespace `pg_advisory_lock` across both planning and applying, and
the one step outside it -- seeding the maps -- inserts with
`ON CONFLICT DO NOTHING`. Both are somebody else's promise, which is
exactly why it is worth a test rather than a comment: this is the thing
that would let two service nodes corrupt a schema between them.

Runs concurrent passes in one VM rather than across peers. The
contention is in PostgreSQL, not in Erlang, so separate connections
from one node exercise the same locks as separate nodes would, without
the boot time.

Skips when PostgreSQL is unreachable or unmigrated.
""".

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%% CT Callbacks
-export([all/0, init_per_suite/1, end_per_suite/1]).

%% Test cases
-export([
    test_concurrent_passes_all_succeed/1,
    test_concurrent_passes_apply_each_script_once/1,
    test_concurrent_passes_do_not_duplicate_seed_data/1
]).

%% Runs in the spawned workers
-export([pass/1]).

-define(PASSES, 3).
-define(NAMESPACES, [<<"main">>, <<"repeatable">>, <<"init">>, <<"test">>]).

all() ->
    [
        test_concurrent_passes_all_succeed,
        test_concurrent_passes_apply_each_script_once,
        test_concurrent_passes_do_not_duplicate_seed_data
    ].

init_per_suite(Config) ->
    ok = application:set_env(lyceum_cluster, node_types, [service]),
    ok = application:set_env(database, root_dir, server_root()),
    case application:ensure_all_started(database) of
        {ok, Started} ->
            case migrator_available() of
                true ->
                    [{started, Started} | Config];
                false ->
                    _ = [application:stop(App) || App <- lists:reverse(Started)],
                    {skip, "PostgreSQL is not reachable, skipping migration passes"}
            end;
        {error, Reason} ->
            {skip, lists:flatten(io_lib:format("database app unavailable: ~p", [Reason]))}
    end.

end_per_suite(Config) ->
    case ?config(started, Config) of
        undefined -> ok;
        Started -> _ = [application:stop(App) || App <- lists:reverse(Started)], ok
    end,
    ok = application:unset_env(lyceum_cluster, node_types),
    ok = application:unset_env(database, root_dir),
    Config.

%%--------------------------------------------------------------------
%% Test Cases
%%--------------------------------------------------------------------

test_concurrent_passes_all_succeed(_Config) ->
    Results = concurrent_passes(?PASSES),

    %% Every pass has to finish. A pass that blocked on another's
    %% advisory lock and gave up would show here as a timeout, which is
    %% the failure mode a lock-free implementation would produce under
    %% load.
    ?assertEqual(?PASSES, length(Results)),
    ?assertEqual([], [R || R <- Results, R =/= ok]).

test_concurrent_passes_apply_each_script_once(_Config) ->
    _ = concurrent_passes(?PASSES),

    %% The journal is the record of what ran. A script applied twice
    %% would leave two in-force entries for one name, which is what a
    %% lost race looks like after the fact.
    Conn = migrator_connection(),
    try
        _ = [assert_no_duplicate_entries(Conn, Namespace) || Namespace <- ?NAMESPACES]
    after
        database:close_migrator_connection(Conn)
    end.

test_concurrent_passes_do_not_duplicate_seed_data(_Config) ->
    %% Seeding the maps is the one step outside migraterl's lock, so it
    %% carries its own protection (ON CONFLICT DO NOTHING) and this is
    %% what checks it still does.
    Before = {count("map.tile"), count("map.object")},
    _ = concurrent_passes(?PASSES),
    ?assertEqual(Before, {count("map.tile"), count("map.object")}).

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

-doc "One migration pass on its own connection, as a separate service node would run it.".
pass(Parent) ->
    Result =
        case database:open_migrator_connection() of
            {ok, Conn} ->
                try
                    world_migrations:run_migrations(Conn)
                after
                    database:close_migrator_connection(Conn)
                end;
            {error, Reason} ->
                {error, Reason}
        end,
    Parent ! {pass, self(), Result}.

concurrent_passes(N) ->
    Parent = self(),
    Workers = [spawn_monitor(?MODULE, pass, [Parent]) || _ <- lists:seq(1, N)],
    collect(Workers, []).

collect([], Acc) ->
    Acc;
collect([{Pid, Ref} | Rest], Acc) ->
    receive
        {pass, Pid, Result} ->
            receive
                {'DOWN', Ref, process, Pid, _} -> ok
            after 5000 -> ct:fail({pass_never_exited, Pid})
            end,
            collect(Rest, [Result | Acc]);
        {'DOWN', Ref, process, Pid, Reason} when Reason =/= normal ->
            ct:fail({pass_crashed, Pid, Reason})
    after 60000 ->
        ct:fail({pass_timeout, Pid})
    end.

assert_no_duplicate_entries(Conn, Namespace) ->
    {ok, Entries} = migraterl:status(Conn, Namespace),
    Names = [maps:get(name, Entry) || Entry <- Entries],
    ?assertEqual(
        lists:sort(lists:usort(Names)),
        lists:sort(Names),
        {duplicate_journal_entries, Namespace}
    ).

count(Table) ->
    #{rows := [#{n := N}]} = database:query(lyceum_pool, "SELECT count(*) AS n FROM " ++ Table),
    N.

migrator_available() ->
    case database:open_migrator_connection() of
        {ok, Conn} ->
            database:close_migrator_connection(Conn),
            true;
        {error, _} ->
            false
    end.

migrator_connection() ->
    {ok, Conn} = database:open_migrator_connection(),
    Conn.

server_root() ->
    lists:foldl(
        fun(_, Path) -> filename:dirname(Path) end,
        filename:absname(code:lib_dir(database)),
        lists:seq(1, 4)
    ).
