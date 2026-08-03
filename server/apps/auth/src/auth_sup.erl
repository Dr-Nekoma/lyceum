-module(auth_sup).
-moduledoc """
Auth supervisor. Everything under it is frontend-only: it is the layer
the Zig client actually talks to, so on logic and service nodes this
supervisor boots empty.
""".

-behaviour(supervisor).

%% API
-export([start_link/0]).
%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

-spec start_link() -> supervisor:startlink_ret().
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================
-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    SupFlags =
        #{
            % simple_auth dispatches every login into client_proxy_sup,
            % so it must not run while the proxy supervisor is down:
            % rest_for_one with the proxy sup first restarts simple_auth
            % whenever the proxy sup goes down.
            strategy => rest_for_one,
            intensity => 12,
            period => 3600
        },

    SimpleAuthWorker =
        #{
            id => simple_auth,
            start => {simple_auth, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [simple_auth]
        },

    ProxySup =
        #{
            id => client_proxy_sup,
            start => {client_proxy_sup, start_link, []},
            restart => permanent,
            shutdown => infinity,
            type => supervisor,
            modules => [client_proxy_sup]
        },

    Specs = [{frontend, ProxySup}, {frontend, SimpleAuthWorker}],
    Children = [Spec || {Layer, Spec} <- Specs, lyceum_cluster:hosts_layer(Layer)],

    logger:info("[~p] Starting Supervisor...~n", [?SERVER]),
    {ok, {SupFlags, Children}}.
