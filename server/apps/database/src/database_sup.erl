-module(database_sup).
-moduledoc """
Top Database Supervisor. Owns the pgo pools (one per role),
that the rest of the umbrella reaches into for queries.
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
    PoolSpecs = lists:map(fun pool_child_spec/1, database:pool_configs()),
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
