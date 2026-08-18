-module(lyceum_service_sup).
-moduledoc """
Supervises the pool of `lyceum_service_worker` processes.

The pool size comes from the `workers` application env. Workers are
identical and stateless, so plain one_for_one is enough: losing one
worker never invalidates another.

Workers must be co-located with the pgo pools and the session cache
they drive, so they only run on `service` nodes. Everywhere else this
supervisor boots empty and `lyceum_service` reaches a remote worker
through `pg`.
""".

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-spec start_link() -> supervisor:startlink_ret().
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    Workers = application:get_env(lyceum_service, workers, 8),
    Flags = #{
        strategy => one_for_one,
        intensity => 12,
        period => 3600
    },
    Specs = [{service, worker_spec(N)} || N <- lists:seq(1, Workers)],
    Children = [Spec || {Layer, Spec} <- Specs, lyceum_cluster:hosts_layer(Layer)],
    {ok, {Flags, Children}}.

-spec worker_spec(pos_integer()) -> supervisor:child_spec().
worker_spec(N) ->
    #{
        id => {lyceum_service_worker, N},
        start => {lyceum_service_worker, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [lyceum_service_worker]
    }.
