%%%-------------------------------------------------------------------
%% @doc Auth supervisor.
%% @end
%%%-------------------------------------------------------------------
-module(auth_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).
%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================
init([]) ->
    SupFlags =
        #{
            strategy => one_for_one,
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

    logger:info("[~p] Starting Supervisor...~n", [?SERVER]),
    {ok, {SupFlags, [SimpleAuthWorker]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
