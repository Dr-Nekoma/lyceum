-module(client_proxy).
-moduledoc """
Per-session gateway between the Zig client and its player process.

The Zig client holds a single distribution connection, to the
frontend node, and zerl never accepts incoming connections, so a pid
living on any other node can neither receive from nor send to the
client. Every client-facing pid therefore has to live on the frontend:
this process is that pid. The client treats it as the opaque "handler"
it already expects after login, so no client changes are needed.

Message flow:

- Anything the client sends lands here and is forwarded verbatim to
  the player process on the logic node.
- The player sends `{reply, Msg}`; the wrapper is stripped and `Msg`
  goes to the client. The tag exists so client-bound traffic can never
  be confused with client-originated traffic.
- `{kick, Reason}` (from a newer login of the same player) informs the
  client and stops the proxy; the player notices via its monitor.

The proxy monitors the player process and the client's node; either
side disappearing tears the session down. The player monitors the
proxy right back, so no orphan survives on the logic node.
""".

-behaviour(gen_server).

-export([start_link/1]).
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_continue/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-record(state, {
    client_pid :: pid(),
    client_node :: node(),
    player_pid :: pid() | undefined,
    request :: map() | undefined
}).

-type state() :: #state{}.

-spec start_link(#{client_pid := pid(), request := map()}) -> gen_server:start_ret().
start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec init(#{client_pid := pid(), request := map()}) ->
    {ok, state(), {continue, login}}.
init(#{client_pid := ClientPid, request := Request}) ->
    State = #state{
        client_pid = ClientPid,
        client_node = node(ClientPid),
        request = Request
    },
    {ok, State, {continue, login}}.

-spec handle_continue(login, state()) ->
    {noreply, state()} | {stop, normal, state()}.
handle_continue(login, #state{client_pid = ClientPid, request = Request} = State) ->
    case player_session:login(Request#{client_pid => self()}) of
        {ok, {PlayerPid, Email}} ->
            _ = monitor(process, PlayerPid),
            true = monitor_node(State#state.client_node, true),
            ClientPid ! {ok, {self(), Email}},
            {noreply, State#state{player_pid = PlayerPid, request = undefined}};
        {error, Reason} ->
            logger:notice("[~p] Login refused: ~p~n", [?MODULE, Reason]),
            ClientPid ! {error, format_error(Reason)},
            {stop, normal, State}
    end.

-spec handle_call(term(), gen_server:from(), state()) -> {reply, ok, state()}.
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(Msg, State) ->
    logger:error("[~p] CAST: ~p~n", [?MODULE, Msg]),
    {noreply, State}.

-spec handle_info(term(), state()) ->
    {noreply, state()} | {stop, normal, state()}.
handle_info({reply, Msg}, #state{client_pid = ClientPid} = State) ->
    ClientPid ! Msg,
    {noreply, State};
handle_info({kick, Reason}, #state{client_pid = ClientPid} = State) ->
    logger:notice("[~p] Session kicked: ~p~n", [?MODULE, Reason]),
    ClientPid ! {error, format_error(Reason)},
    {stop, normal, State};
handle_info({'DOWN', _Ref, process, PlayerPid, normal}, #state{player_pid = PlayerPid} = State) ->
    %% Player finished normally (logout); nothing left to relay.
    {stop, normal, State};
handle_info({'DOWN', _Ref, process, PlayerPid, Reason}, #state{player_pid = PlayerPid} = State) ->
    logger:warning("[~p] Player ~p died: ~p~n", [?MODULE, PlayerPid, Reason]),
    State#state.client_pid ! {error, "Session lost"},
    {stop, normal, State};
handle_info({nodedown, Node}, #state{client_node = Node} = State) ->
    %% The client's C-node connection is gone. The player notices this
    %% proxy stopping through its own monitor and cleans up.
    logger:info("[~p] Client node ~p disconnected~n", [?MODULE, Node]),
    {stop, normal, State};
handle_info(ClientMsg, #state{player_pid = PlayerPid} = State) when is_pid(PlayerPid) ->
    PlayerPid ! ClientMsg,
    {noreply, State};
handle_info(Msg, State) ->
    logger:warning("[~p] Dropping message with no player attached: ~p~n", [?MODULE, Msg]),
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

%% The Zig client renders the error payload as text, so atoms coming
%% from the routing layers are translated to readable strings; strings
%% from the service layer pass through untouched.
-spec format_error(term()) -> string().
format_error(Reason) when is_list(Reason) ->
    Reason;
format_error(no_logic) ->
    "No game server available";
format_error(logic_timeout) ->
    "Game server timed out";
format_error(no_service) ->
    "Game services unavailable";
format_error(service_timeout) ->
    "Game services timed out";
format_error(Reason) ->
    lists:flatten(io_lib:format("~p", [Reason])).
