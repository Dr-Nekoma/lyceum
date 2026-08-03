-module(lyceum_cluster_connector).
-moduledoc """
Keeps this node connected to its configured peers, and advertises what
kind of node it is.

Two responsibilities, both of which have to survive the network being
unavailable:

1. Join the `{node_type, T}` process group, which is how
   `lyceum_cluster:nodes_of_type/1` enumerates the cluster.
2. Connect to every node in the `peers` configuration and reconnect
   whenever one of them goes away.

Following the same principle as `world_migrations`, `init/1` never
blocks on nor crashes because of something outside the VM. Connection
attempts happen in `handle_continue/2` and are retried with capped
exponential backoff, so an unreachable peer can never burn the
supervisor's restart intensity at boot.

Peers are a convenience, not a requirement. A node with no reachable
peers still boots and serves whatever it can locally, which is exactly
what layering is supposed to buy: the frontend stays up and reports a
useful error when the logic layer is gone.
""".

-behaviour(gen_server).

-export([start_link/0, status/0]).
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_continue/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-define(SERVER, ?MODULE).

-record(state, {
    node_types :: [lyceum_cluster:node_type(), ...],
    attempts = 0 :: non_neg_integer(),
    retry :: undefined | reference()
}).

-type state() :: #state{}.

%%%===================================================================
%%% API
%%%===================================================================
-spec start_link() -> gen_server:start_ret().
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-doc """
Reports what this node believes about the cluster. Intended for a
remote shell, not for routing decisions, use `lyceum_cluster:pick/1`
for those.
""".
-spec status() -> #{
    node := node(),
    node_types := [lyceum_cluster:node_type(), ...],
    peers := [node()],
    connected := [node()],
    missing := [node()]
}.
status() ->
    gen_server:call(?SERVER, status).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
-doc """
Registers this node's layers and hands the connection work to
`handle_continue/2`.

`net_kernel:monitor_nodes/2` is asked for `all` node types so that
hidden connections are reported too. Test clusters are commonly wired
up with hidden nodes, and silently ignoring them would make the
connector's view of the cluster disagree with reality.
""".
-spec init(list()) -> {ok, state(), {continue, connect}}.
init([]) ->
    Types = lyceum_cluster:node_types(),
    _ = [ok = lyceum_cluster:join({node_type, T}) || T <- Types],
    case is_distributed() of
        true ->
            ok = net_kernel:monitor_nodes(true, [{node_type, all}]);
        false ->
            logger:warning(
                "[~p] Node is not distributed, running standalone~n",
                [?SERVER]
            )
    end,
    {ok, #state{node_types = Types}, {continue, connect}}.

-doc """
Connects to every configured peer that is not connected yet, and
schedules a retry when some are still missing.
""".
-spec handle_continue(connect, state()) -> {noreply, state()}.
handle_continue(connect, State) ->
    case missing_peers() of
        [] ->
            {noreply, State#state{attempts = 0, retry = undefined}};
        Missing ->
            _ = [connect(Node) || Node <- Missing],
            case missing_peers() of
                [] ->
                    logger:info("[~p] Connected to every peer~n", [?SERVER]),
                    {noreply, State#state{attempts = 0, retry = undefined}};
                Remaining ->
                    {noreply, schedule_retry(Remaining, State)}
            end
    end.

-spec handle_call(term(), gen_server:from(), state()) -> {reply, term(), state()}.
handle_call(status, _From, State) ->
    Reply =
        #{
            node => node(),
            node_types => State#state.node_types,
            peers => lyceum_cluster:peers(),
            connected => connected_nodes(),
            missing => missing_peers()
        },
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-doc """
Reacts to the cluster changing shape.

A `nodedown` for a configured peer starts the reconnection loop again.
A `nodedown` for anything else is only worth logging, since nodes we
did not configure are free to come and go.
""".
-spec handle_info(term(), state()) -> {noreply, state()} | {noreply, state(), {continue, connect}}.
handle_info({nodeup, Node, _Info}, State) ->
    logger:info("[~p] Node ~p is up~n", [?SERVER, Node]),
    {noreply, State};
handle_info({nodedown, Node, _Info}, State) ->
    case lists:member(Node, lyceum_cluster:peers()) of
        true ->
            logger:warning("[~p] Peer ~p went down, reconnecting~n", [?SERVER, Node]),
            {noreply, cancel_retry(State#state{attempts = 0}), {continue, connect}};
        false ->
            logger:info("[~p] Node ~p is down~n", [?SERVER, Node]),
            {noreply, State}
    end;
handle_info({retry, Ref}, #state{retry = Ref} = State) ->
    {noreply, State#state{retry = undefined}, {continue, connect}};
handle_info({retry, _Stale}, State) ->
    % A retry from a loop that has already been superseded, for example
    % by a nodedown arriving while it was in flight.
    {noreply, State};
handle_info(Info, State) ->
    logger:info("[~p] INFO: ~p~n", [?SERVER, Info]),
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, _State) ->
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec is_distributed() -> boolean().
is_distributed() ->
    node() =/= 'nonode@nohost'.

-doc """
Connected nodes including hidden ones. Plain `nodes/0` omits hidden
connections, which would make the connector retry forever against a
peer it is in fact already talking to.
""".
-spec connected_nodes() -> [node()].
connected_nodes() ->
    nodes(connected).

-spec missing_peers() -> [node()].
missing_peers() ->
    case is_distributed() of
        false ->
            [];
        true ->
            Connected = connected_nodes(),
            [N || N <- lyceum_cluster:peers(), N =/= node(), not lists:member(N, Connected)]
    end.

-spec connect(node()) -> boolean() | ignored.
connect(Node) ->
    logger:debug("[~p] Connecting to ~p...~n", [?SERVER, Node]),
    net_kernel:connect_node(Node).

-spec schedule_retry([node()], state()) -> state().
schedule_retry(Missing, State) ->
    Attempts = State#state.attempts + 1,
    Delay = retry_delay(Attempts),
    logger:warning(
        "[~p] Peers ~p unreachable, retrying in ~pms (attempt ~p)~n",
        [?SERVER, Missing, Delay, Attempts]
    ),
    Ref = make_ref(),
    _ = erlang:send_after(Delay, self(), {retry, Ref}),
    State#state{attempts = Attempts, retry = Ref}.

-doc """
Invalidates any retry already in flight by dropping its reference. The
timer itself is left alone, `handle_info/2` discards the message when
the reference no longer matches.
""".
-spec cancel_retry(state()) -> state().
cancel_retry(State) ->
    State#state{retry = undefined}.

-spec retry_delay(pos_integer()) -> pos_integer().
retry_delay(Attempts) ->
    Base = application:get_env(lyceum_cluster, connect_retry_base_ms, 1000),
    Max = application:get_env(lyceum_cluster, connect_retry_max_ms, 30000),
    lyceum_cluster_backoff:delay(Attempts, Base, Max).
