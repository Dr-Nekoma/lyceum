-module(cache_sup).
-moduledoc """
Session store top level supervisor.

`cache` itself no longer has a process: sessions live in Postgres and
the module runs inside whichever `lyceum_service_worker` called it, so
that `pgo` keeps the transaction on the pool's node. What is left to
supervise is `session_reaper`, which closes sessions whose logic node
died without logging anyone out.

That is still a service-layer concern -- it needs the pool, and it
watches the logic nodes that call in -- so on frontend and logic nodes
this supervisor boots empty.
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
    SupFlags =
        #{strategy => one_for_one,
          intensity => 10,
          period => 600},

    Reaper =
        #{id => session_reaper,
          start => {session_reaper, start_link, []},
          restart => permanent,
          shutdown => 5000,
          type => worker,
          modules => [session_reaper]},

    Children = [Spec || {Layer, Spec} <- [{service, Reaper}], lyceum_cluster:hosts_layer(Layer)],
    {ok, {SupFlags, Children}}.
