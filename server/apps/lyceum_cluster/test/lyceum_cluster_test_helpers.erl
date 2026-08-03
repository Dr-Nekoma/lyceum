-module(lyceum_cluster_test_helpers).
-moduledoc """
Helpers shared by the `lyceum_cluster` suites.

Kept out of the suites themselves so the example-based tests and the
properties can agree on what "a worker joined to a group" means without
either one depending on the other.
""".

-export([start_scope/0, stop_scope/1]).
-export([spawn_worker/1, spawn_responder/2, stop_worker/1, wait_until/1, wait_until/2]).
-export([peer_supported/0, start_peer/1, stop_peer/1]).

-doc """
Starts the `pg` scope the group API reads from.

The suites drive `lyceum_cluster` directly rather than booting the
whole application, so nothing else brings the scope up.
""".
-spec start_scope() -> pid().
start_scope() ->
    {ok, Pid} = pg:start_link(lyceum_cluster:scope()),
    Pid.

-spec stop_scope(pid()) -> ok.
stop_scope(Pid) ->
    gen_server:stop(Pid).

-doc """
Spawns a process that joins `Group` and then idles.

Returns only once the join is visible, so a caller can immediately
assert on group membership without sleeping.
""".
-spec spawn_worker(lyceum_cluster:group()) -> pid().
spawn_worker(Group) ->
    Parent = self(),
    Pid = spawn(fun() ->
                   ok = lyceum_cluster:join(Group),
                   Parent ! {joined, self()},
                   receive
                       stop -> ok
                   end
                end),
    receive
        {joined, Pid} -> Pid
    after 1000 -> error({worker_did_not_join, Group})
    end.

-doc """
Like `spawn_worker/1`, but the process also answers `gen_server:call`
with `Fun(Request)`.

Enough of a gen_server for `lyceum_cluster:call/3` to talk to, without
a callback module per test: a `Fun` that sleeps produces a timeout, one
that returns a term produces a reply.
""".
-spec spawn_responder(lyceum_cluster:group(), fun((term()) -> term())) -> pid().
spawn_responder(Group, Fun) ->
    Parent = self(),
    Pid = spawn(fun() ->
                   ok = lyceum_cluster:join(Group),
                   Parent ! {joined, self()},
                   responder_loop(Fun)
                end),
    receive
        {joined, Pid} -> Pid
    after 1000 -> error({responder_did_not_join, Group})
    end.

-spec responder_loop(fun((term()) -> term())) -> ok.
responder_loop(Fun) ->
    receive
        {'$gen_call', From, Request} ->
            gen_server:reply(From, Fun(Request)),
            responder_loop(Fun);
        stop ->
            ok
    end.

-doc "Stops a worker and waits for it to actually be gone.".
-spec stop_worker(pid()) -> ok.
stop_worker(Pid) ->
    Ref = monitor(process, Pid),
    Pid ! stop,
    receive
        {'DOWN', Ref, process, Pid, _} -> ok
    after 1000 -> error({worker_did_not_stop, Pid})
    end.

-doc """
Polls until `Fun` returns true.

`pg` membership is eventually consistent even within a single node,
since it is maintained by a separate process reacting to monitors.
Asserting on it directly after a process dies is a race.
""".
-spec wait_until(fun(() -> boolean())) -> ok.
wait_until(Fun) ->
    wait_until(Fun, 50).

-doc "Polls `Fun` at most `Retries` times, 20ms apart.".
-spec wait_until(fun(() -> boolean()), non_neg_integer()) -> ok.
wait_until(_Fun, 0) ->
    error(condition_never_held);
wait_until(Fun, Retries) ->
    case Fun() of
        true ->
            ok;
        false ->
            timer:sleep(20),
            wait_until(Fun, Retries - 1)
    end.

%%%===================================================================
%%% Multi-node helpers
%%%===================================================================
-doc """
Whether this environment can start peer nodes at all.

Distribution needs a reachable epmd, which a sufficiently locked-down
build sandbox may not have. Suites use this to skip rather than fail,
since "epmd is unavailable here" is not a defect in the code under
test.
""".
-spec peer_supported() -> boolean().
peer_supported() ->
    case peer:start(#{name => peer:random_name(?MODULE), connection => standard_io}) of
        {ok, Peer, _Node} ->
            ok = peer:stop(Peer),
            true;
        {error, _Reason} ->
            false
    end.

-doc """
Starts a Lyceum node in its own VM.

`connection => standard_io` keeps the test node out of the cluster:
control traffic runs over the port rather than over distribution, so
the only visible links that exist are the ones the topology under test
asked for. A CT node quietly connected to all three layers would make
every isolation assertion here meaningless.

Configuration is set through the *string* forms on purpose. That is
what `sys.config.src` produces from the environment, so a real boot
exercises the same parsing a deployment does.

It travels on the command line rather than through
`application:set_env/3`, because `application:load/1` overwrites
anything set beforehand with the defaults from the `.app` file: a
`peers` set that way is silently back to `[]` by the time the connector
reads it. Command-line values survive the load, which is also how a
node without a `-config` file is configured for real.

`Spec` is `#{name, node_types, peers => [node()], apps => [atom()],
env => [{App, Key, Value}]}`.
""".
-spec start_peer(map()) -> {pid(), node()}.
start_peer(#{name := Name} = Spec) ->
    Env =
        [
            {lyceum_cluster, node_types, maps:get(node_types, Spec)},
            {lyceum_cluster, peers, string:join([atom_to_list(P) || P <- peers(Spec)], ",")}
        ] ++ maps:get(env, Spec, []),
    Args =
        ["-pa"] ++
            code:get_path() ++
            ["-setcookie", "lyceum", "-kernel", "connect_all", "false"] ++
            lists:append([env_flag(E) || E <- Env]),
    {ok, Peer, Node} =
        peer:start(#{
            name => Name,
            connection => standard_io,
            args => Args,
            wait_boot => 15000
        }),
    _ = [
        {ok, _} = peer:call(Peer, application, ensure_all_started, [App])
     || App <- maps:get(apps, Spec, [lyceum_cluster])
    ],
    {Peer, Node}.

-spec env_flag({atom(), atom(), term()}) -> [string()].
env_flag({App, Key, Value}) ->
    ["-" ++ atom_to_list(App), atom_to_list(Key), lists:flatten(io_lib:format("~p", [Value]))].

-spec peers(map()) -> [node()].
peers(Spec) ->
    maps:get(peers, Spec, []).

-doc "Stops a peer node, tolerating one that is already gone.".
-spec stop_peer(pid()) -> ok.
stop_peer(Peer) ->
    try
        peer:stop(Peer)
    catch
        exit:_ -> ok
    end.
