-module(player_root_sup).
-moduledoc """
Root supervisor of the player app: the session manager plus the
dynamic supervisor holding every player process.

one_for_one is enough, `player_session` is stateless (sessions live
in the service-layer cache), so restarting it does not invalidate
running players, and vice versa.

Both children are logic-only, so on frontend and service nodes this
supervisor boots empty.
""".

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-spec start_link() -> supervisor:startlink_ret().
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    Flags = #{
        strategy => one_for_one,
        intensity => 12,
        period => 3600
    },
    Session = #{
        id => player_session,
        start => {player_session, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [player_session]
    },
    TopLevel = #{
        id => player_top_level_sup,
        start => {player_top_level_sup, start_link, []},
        restart => permanent,
        shutdown => infinity,
        type => supervisor,
        modules => [player_top_level_sup]
    },
    Specs = [{logic, Session}, {logic, TopLevel}],
    Children = [Spec || {Layer, Spec} <- Specs, lyceum_cluster:hosts_layer(Layer)],
    {ok, {Flags, Children}}.
