-module(lyceum_cluster_prop_SUITE).

%% Deliberately does not include eunit.hrl. It and proper.hrl both
%% define ?LET, and the properties only ever want proper's.
-include_lib("common_test/include/ct.hrl").
-include_lib("proper/include/proper.hrl").

-import(lyceum_cluster_test_helpers,
        [start_scope/0, stop_scope/1, spawn_worker/1, stop_worker/1]).

%% CT Callbacks
-export([all/0, groups/0, init_per_group/2, end_per_group/2, init_per_testcase/2,
         end_per_testcase/2]).
%% Test cases
-export([test_ceiling_is_bounded/1, test_ceiling_is_monotonic/1, test_ceiling_saturates/1,
         test_delay_respects_ceiling/1, test_pick_always_returns_a_member/1]).
-export([test_node_types_round_trip/1, test_peers_round_trip/1]).

-define(NUM_TESTS, 500).

%%--------------------------------------------------------------------
%% CT Callbacks
%%--------------------------------------------------------------------

all() ->
    [{group, backoff}, {group, discovery}, {group, configuration}].

groups() ->
    [{backoff,
      [test_ceiling_is_bounded, test_ceiling_is_monotonic, test_ceiling_saturates,
       test_delay_respects_ceiling]},
     {discovery, [test_pick_always_returns_a_member]},
     {configuration, [test_node_types_round_trip, test_peers_round_trip]}].

init_per_group(Name, Config) ->
    ct:pal("Starting group: ~p~n", [Name]),
    Config.

end_per_group(Name, Config) ->
    ct:pal("Ending Group: ~p~n", [Name]),
    Config.

init_per_testcase(TestCase, Config) ->
    ok = application:set_env(lyceum_cluster, node_types, [frontend, logic, service]),
    Pid = start_scope(),
    ct:pal("[~p] pg scope started at ~p", [TestCase, Pid]),
    [{scope_pid, Pid} | Config].

end_per_testcase(_TestCase, Config) ->
    stop_scope(?config(scope_pid, Config)),
    ok = application:unset_env(lyceum_cluster, node_types),
    Config.

%%--------------------------------------------------------------------
%% Test Cases
%%--------------------------------------------------------------------
%% The backoff is a pure function over three integers, which is the one
%% place in this app where enumerating cases by hand is strictly worse
%% than generating them.
test_ceiling_is_bounded(_Config) ->
    quickcheck(prop_ceiling_is_bounded()).

test_ceiling_is_monotonic(_Config) ->
    quickcheck(prop_ceiling_is_monotonic()).

test_ceiling_saturates(_Config) ->
    quickcheck(prop_ceiling_saturates()).

test_delay_respects_ceiling(_Config) ->
    quickcheck(prop_delay_respects_ceiling()).

test_pick_always_returns_a_member(_Config) ->
    %% Fewer runs than the pure properties, each one spawns and reaps
    %% real processes.
    quickcheck(prop_pick_returns_a_member(), 50).

test_node_types_round_trip(_Config) ->
    quickcheck(prop_node_types_round_trip()).

test_peers_round_trip(_Config) ->
    %% Fewer runs than the pure properties: every generated peer name
    %% costs an atom, and atoms are never reclaimed.
    quickcheck(prop_peers_round_trip(), 100).

%%--------------------------------------------------------------------
%% Properties
%%--------------------------------------------------------------------
%% A delay is never longer than the configured maximum and never
%% shorter than a single millisecond, whatever the caller configures.
%% The upper bound is the one that matters, an unbounded backoff would
%% mean a peer that recovers is never noticed.
prop_ceiling_is_bounded() ->
    ?FORALL({Attempt, Base, Max},
            {pos_integer(), pos_integer(), pos_integer()},
            begin
                Ceiling = lyceum_cluster_backoff:ceiling(Attempt, Base, Max),
                Ceiling >= 1 andalso Ceiling =< Max andalso Ceiling =< Base bsl 8
            end).

%% Waiting longer after more consecutive failures is the entire point
%% of backing off, so the ceiling must never decrease as attempts pile
%% up.
prop_ceiling_is_monotonic() ->
    ?FORALL({Attempt, Base, Max},
            {pos_integer(), pos_integer(), pos_integer()},
            lyceum_cluster_backoff:ceiling(Attempt + 1, Base, Max)
            >= lyceum_cluster_backoff:ceiling(Attempt, Base, Max)).

%% Past the shift clamp the ceiling stops moving. This is what stops a
%% peer that has been down for a week from computing a bignum delay.
prop_ceiling_saturates() ->
    ?FORALL({Extra, Base, Max},
            {non_neg_integer(), pos_integer(), pos_integer()},
            lyceum_cluster_backoff:ceiling(9 + Extra, Base, Max)
            =:= min(Base bsl 8, Max)).

%% Full jitter picks uniformly below the ceiling, so an actual delay is
%% free to be short but never exceeds the bound the ceiling promises.
prop_delay_respects_ceiling() ->
    ?FORALL({Attempt, Base, Max},
            {pos_integer(), pos_integer(), pos_integer()},
            begin
                Delay = lyceum_cluster_backoff:delay(Attempt, Base, Max),
                Delay >= 1 andalso Delay =< lyceum_cluster_backoff:ceiling(Attempt, Base, Max)
            end).

%% The invariant every *_api module depends on: for any non-empty set
%% of workers, pick/1 hands back one of them, and never no_service.
%%
%% Each run uses a fresh group name so members cannot leak between runs
%% and make a later run pass for the wrong reason.
prop_pick_returns_a_member() ->
    ?FORALL(Count,
            range(1, 8),
            begin
                Group = {test_group, erlang:unique_integer()},
                Workers = [spawn_worker(Group) || _ <- lists:seq(1, Count)],
                Registered = length(lyceum_cluster:members(Group)),
                Picked = [lyceum_cluster:pick(Group) || _ <- lists:seq(1, 20)],
                _ = [stop_worker(W) || W <- Workers],
                Registered =:= Count
                andalso lists:all(fun(P) -> lists:member(P, Workers) end, Picked)
            end).

%% Whatever a deployment writes into $LYCEUM_NODE_TYPES, the node must
%% end up hosting exactly the layers that were asked for. The Erlang
%% term form is the reference answer the string form has to match.
prop_node_types_round_trip() ->
    ?FORALL({Types, Separator},
            {non_empty(list(oneof([frontend, logic, service]))), separator()},
            begin
                String = lists:join(Separator, [atom_to_list(T) || T <- Types]),
                ok = application:set_env(lyceum_cluster, node_types, lists:flatten(String)),
                lyceum_cluster:node_types() =:= lists:usort(Types)
            end).

%% Same contract for the peer list, plus the empty case: a node with no
%% layer below it is configured with an empty string, not a missing key.
prop_peers_round_trip() ->
    ?FORALL({Peers, Separator},
            {list(peer()), separator()},
            begin
                String = lists:join(Separator, [atom_to_list(P) || P <- Peers]),
                ok = application:set_env(lyceum_cluster, peers, lists:flatten(String)),
                lyceum_cluster:peers() =:= Peers
            end).

%%--------------------------------------------------------------------
%% Generators
%%--------------------------------------------------------------------
%% Commas, with any amount of the whitespace a human editing a unit
%% file or a compose file would leave behind.
separator() ->
    ?LET({Before, After},
         {list(oneof([$\s, $\t])), list(oneof([$\s, $\t]))},
         Before ++ "," ++ After).

peer() ->
    ?LET({Name, Host},
         {node_name(), node_name()},
         list_to_atom(Name ++ "@" ++ Host)).

node_name() ->
    non_empty(list(oneof(lists:seq($a, $z) ++ lists:seq($0, $9) ++ [$_]))).

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------
quickcheck(Property) ->
    quickcheck(Property, ?NUM_TESTS).

quickcheck(Property, NumTests) ->
    case proper:quickcheck(Property, [{numtests, NumTests}, {to_file, user}]) of
        true ->
            ok;
        Counterexample ->
            ct:fail({property_failed, Counterexample})
    end.
