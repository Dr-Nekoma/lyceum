%%%-------------------------------------------------------------------
%% @doc The client's front door: a thin login dispatcher.
%%
%% Registered as `lyceum_server' because that is the name the Zig
%% client sends its first message to. Its only job is to spawn a
%% client_proxy per login attempt; everything after that first message
%% flows through the proxy, never through this process.
%% @end
%%%-------------------------------------------------------------------
-module(simple_auth).

-behaviour(gen_server).

%% API
-export([start_link/0]).
%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

% For legacy reasons, the client needs to send the first
% request to a "lyceum_server", which got broken into
% multiple processes. Nowadays we only need it for initial
% commnunications with this dispatcher gen_server.
-define(SERVER, lyceum_server).

-include("auth_state.hrl").

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link() -> gen_server:start_ret().
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec init(Args) -> Result when
    Args :: list(),
    State :: auth_state(),
    Success :: {ok, State},
    SuccessWithTimeout :: {ok, State, Timeout :: timeout()},
    Result :: Success | SuccessWithTimeout.
init(_) ->
    Pid = self(),
    logger:info("[~p] Starting at ~p...~n", [?SERVER, Pid]),
    State = #auth_state{pid = Pid},
    {ok, State}.

-spec handle_call(term(), gen_server:from(), auth_state()) -> Result when
    Result :: {reply, term(), auth_state()}.
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

-spec handle_cast(term(), auth_state()) -> Result when
    Result :: {noreply, auth_state()}.
handle_cast(Msg, State) ->
    logger:error("[~p] CAST: ~p~n", [?SERVER, Msg]),
    {noreply, State}.

-spec handle_info(term(), auth_state()) -> Result when
    Result :: {noreply, auth_state()}.
handle_info({From, {login, Request}}, State) when is_pid(From) ->
    logger:info("[~p] Login attempt from ~p~n", [?SERVER, From]),
    ok = dispatch_login(From, Request),
    {noreply, State};
handle_info(Info, State) ->
    logger:error("[~p] INFO: ~p~n", [?SERVER, Info]),
    {noreply, State}.

-spec dispatch_login(pid(), map()) -> ok.
dispatch_login(From, Request) ->
    case client_proxy_sup:start_child(#{client_pid => From, request => Request}) of
        {ok, _ProxyPid} ->
            ok;
        {error, Reason} ->
            logger:error("[~p] Could not start a session: ~p~n", [?SERVER, Reason]),
            From ! {error, "Failed to start a session"},
            ok
    end.

-spec terminate(term(), auth_state()) -> ok.
terminate(_Reason, _State) ->
    ok.

-spec code_change(term(), auth_state(), term()) -> {ok, auth_state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
