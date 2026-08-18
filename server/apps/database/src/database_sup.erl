-module(database_sup).
-moduledoc """
Top Database Supervisor. Owns the pgo pools (one per role),
that the rest of the umbrella reaches into for queries.

Only `service` nodes hold pools: everyone else reaches the database
through `lyceum_service`, and starting idle pools on a frontend or
logic node would only burn connections against PostgreSQL. On those
nodes this supervisor boots empty.
""".

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

-spec start_link() -> supervisor:startlink_ret().
start_link() ->
    logger:info("[~p] Starting Supervisor...~n", [?MODULE]),
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    SupFlags = #{strategy => one_for_one, intensity => 3, period => 60},
    Specs = [{service, pool_child_spec(Cfg)} || Cfg <- database:pool_configs()],
    PoolSpecs = [Spec || {Layer, Spec} <- Specs, lyceum_cluster:hosts_layer(Layer)],
    {ok, {SupFlags, PoolSpecs}}.

-spec pool_child_spec(database:pool_config()) -> supervisor:child_spec().
pool_child_spec(#{name := Name} = Cfg) ->
    Options = database:pool_options(Cfg),
    #{
        id => {pgo_pool, Name},
        start => {pgo_pool, start_link, [Name, Options]},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [pgo_pool]
    }.
