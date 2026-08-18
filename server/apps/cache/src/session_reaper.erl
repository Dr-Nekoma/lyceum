-module(session_reaper).
-moduledoc """
Closes sessions whose owner went away without logging out.

A session row outlives the processes it describes. The normal endings
are covered elsewhere -- the player logs out, or its proxy dies and the
player FSM deactivates -- but a *logic node* dying takes its player
FSMs with it and nothing is left to close their rows.

This watches for that. `owner_node` is the logic node precisely because
it is the one the service layer can see: peers point downward, so a
service node is never connected to a frontend node and cannot observe
one going down, but every logic node that calls it is a direct
neighbour.

## Why the periodic sweep is off by default

Reaping on `nodedown` is unambiguous: we were connected to that node
and watched it go. A timer-driven sweep is not. With more than one
service node, this node cannot distinguish "that logic node is dead"
from "that logic node talks to a different service node and was never
mine to see" -- and closing a live player's session because of that
would log them out for no reason.

So the sweep exists for single-service-node deployments, where the
distinction cannot arise, and is disabled unless `sweep_interval_ms` is
set to a number. Leaving it off is safe: a stale row is not a
correctness problem, because a login closes whatever session the player
had before opening the new one. It only makes "who is online" too
generous.
""".

-behaviour(gen_server).

-export([start_link/0, sweep/0]).
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-define(SERVER, ?MODULE).
-define(DEFAULT_STALE_AFTER_SECONDS, 3600).
-define(DEFAULT_BATCH, 500).

-record(state, {
    %% Logic nodes this node has actually been connected to. The sweep
    %% will not touch a node it has never seen: that node belongs to
    %% somebody else.
    seen = sets:new([{version, 2}]) :: sets:set(node())
}).

-type state() :: #state{}.

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link() -> gen_server:start_ret().
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-doc "Runs a sweep now, regardless of the timer. Returns how many sessions it closed.".
-spec sweep() -> {ok, non_neg_integer()} | {error, term()}.
sweep() ->
    gen_server:call(?SERVER, sweep, 30000).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec init([]) -> {ok, state()}.
init([]) ->
    ok = net_kernel:monitor_nodes(true),
    ok = schedule_sweep(),
    {ok, #state{seen = sets:from_list(nodes(), [{version, 2}])}}.

-spec handle_call(term(), gen_server:from(), state()) -> {reply, term(), state()}.
handle_call(sweep, _From, State) ->
    {reply, run_sweep(State), State};
handle_call(Request, _From, State) ->
    logger:error("[~p] Unknown request: ~p~n", [?SERVER, Request]),
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info({nodeup, Node}, State) ->
    {noreply, State#state{seen = sets:add_element(Node, State#state.seen)}};
handle_info({nodedown, Node}, State) ->
    %% Unconditional: the query is keyed on owner_node, so a node that
    %% never owned a session closes nothing. Cheaper than asking which
    %% layer it was, and correct for a node that is already gone.
    case cache:close_sessions_of_node(Node) of
        ok ->
            logger:info("[~p] Closed sessions owned by ~p~n", [?SERVER, Node]);
        {error, Reason} ->
            logger:error(
                "[~p] Could not close sessions owned by ~p: ~p~n",
                [?SERVER, Node, Reason]
            )
    end,
    {noreply, State};
handle_info(sweep, State) ->
    _ = run_sweep(State),
    ok = schedule_sweep(),
    {noreply, State};
handle_info(Info, State) ->
    logger:debug("[~p] INFO: ~p~n", [?SERVER, Info]),
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, _State) ->
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal
%%%===================================================================

-spec run_sweep(state()) -> {ok, non_neg_integer()} | {error, term()}.
run_sweep(#state{seen = Seen}) ->
    StaleAfter = env(stale_after_seconds, ?DEFAULT_STALE_AFTER_SECONDS),
    case cache:stale_sessions(StaleAfter, env(sweep_batch, ?DEFAULT_BATCH)) of
        {ok, Candidates} ->
            Reapable = [
                Candidate
             || {_PlayerId, Owner} = Candidate <- Candidates,
                reapable(Owner, Seen)
            ],
            {ok, close_all(Reapable, StaleAfter, 0)};
        {error, _Reason} = Error ->
            Error
    end.

-doc """
Whether this node is entitled to close sessions owned by `Owner`.

Two conditions, both necessary: we have seen the node connected at some
point (so it is one of ours, not another service node's), and it is not
connected now.
""".
-spec reapable(node(), sets:set(node())) -> boolean().
reapable(Owner, Seen) ->
    Owner =/= node() andalso
        sets:is_element(Owner, Seen) andalso
        not lists:member(Owner, nodes()).

-spec close_all([{integer(), node()}], non_neg_integer(), non_neg_integer()) ->
    non_neg_integer().
close_all([], _StaleAfter, Closed) ->
    Closed;
close_all([{PlayerId, Owner} | Rest], StaleAfter, Closed) ->
    %% close_stale_session/3 repeats the staleness check in the
    %% statement, so a session that stopped being stale since the
    %% candidate list was read closes nothing and counts as skipped.
    case cache:close_stale_session(PlayerId, Owner, StaleAfter) of
        {ok, N} ->
            close_all(Rest, StaleAfter, Closed + N);
        {error, Reason} ->
            logger:error(
                "[~p] Could not close session ~p: ~p~n",
                [?SERVER, PlayerId, Reason]
            ),
            close_all(Rest, StaleAfter, Closed)
    end.

-spec schedule_sweep() -> ok.
schedule_sweep() ->
    case application:get_env(cache, sweep_interval_ms) of
        {ok, Ms} when is_integer(Ms), Ms > 0 ->
            _ = erlang:send_after(Ms, self(), sweep),
            ok;
        _Disabled ->
            ok
    end.

-doc "App env with a type-checked fallback: a bad value must not reach arithmetic.".
-spec env(atom(), pos_integer()) -> pos_integer().
env(Key, Default) ->
    case application:get_env(cache, Key, Default) of
        Value when is_integer(Value), Value > 0 -> Value;
        _Other -> Default
    end.
