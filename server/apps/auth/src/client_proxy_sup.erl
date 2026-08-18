-module(client_proxy_sup).
-moduledoc """
Supervises one `client_proxy` per connected client session.

Children are temporary: a proxy that stops, client disconnect, kick,
player death, must never be restarted, because the pid it was known
by is the client's handler and a restarted process would have a
different pid anyway.
""".

-behaviour(supervisor).

-export([start_link/0, start_child/1]).
-export([init/1]).

-spec start_link() -> supervisor:startlink_ret().
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec start_child(#{client_pid := pid(), request := map()}) ->
    {ok, pid()} | {error, term()}.
start_child(Args) ->
    case supervisor:start_child(?MODULE, [Args]) of
        {ok, Pid} ->
            {ok, Pid};
        {ok, Pid, _Info} ->
            {ok, Pid};
        {error, Reason} ->
            logger:error("[~p] Failed to start proxy: ~p~n", [?MODULE, Reason]),
            {error, Reason}
    end.

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    Flags = #{
        strategy => simple_one_for_one,
        intensity => 10,
        period => 60
    },
    Proxy = #{
        id => client_proxy,
        start => {client_proxy, start_link, []},
        restart => temporary,
        shutdown => 5000,
        type => worker,
        modules => [client_proxy]
    },
    {ok, {Flags, [Proxy]}}.
