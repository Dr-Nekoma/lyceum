-module(player_session).
-moduledoc """
Logic-layer session manager: the frontend's door into the game.

Registered locally and joined to the pg group `logic`, so frontend
nodes reach it with `lyceum_cluster:call(logic, ...)`, never by node
name. `login/1` is the caller-side API and runs on the frontend; the server
side executes on the logic node, where it validates credentials and
registers the session through `lyceum_service`, kicks the previous
session's proxy if there is one, and spawns the player process under
the local `player_top_level_sup`.

The session cache on the service layer is the duplicate-login arbiter:
`lyceum_service:login_session/1` returns the previous session's proxy
pid, and the single cache process serializes concurrent logins for the
same player. No `global` registration is involved anywhere.
""".

-behaviour(gen_server).

-export([start_link/0, login/1]).
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-include("player_state.hrl").

-define(LOGIN_TIMEOUT, 10000).

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link() -> gen_server:start_ret().
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-doc """
Logs a client in. Runs on the calling (frontend) node; the work
happens on whatever logic node `pick(logic)` returns. `client_pid`
must be the client's proxy pid on the frontend, it becomes the pid
the player process replies to and monitors.
""".
-spec login(#{username := _, password := _, client_pid := pid(), _ => _}) ->
    {ok, {pid(), player_email()}} | {error, term()}.
login(Request) ->
    lyceum_cluster:call(logic, {login, Request}, #{
        timeout => ?LOGIN_TIMEOUT,
        unavailable => no_logic,
        timed_out => logic_timeout
    }).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec init([]) -> {ok, no_state}.
init([]) ->
    ok = lyceum_cluster:join(logic),
    {ok, no_state}.

-spec handle_call(term(), gen_server:from(), no_state) -> {reply, term(), no_state}.
handle_call({login, Request}, _From, State) ->
    {reply, do_login(Request), State};
handle_call(Request, _From, State) ->
    logger:error("[~p] Unknown request: ~p~n", [?MODULE, Request]),
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), no_state) -> {noreply, no_state}.
handle_cast(Msg, State) ->
    logger:error("[~p] CAST: ~p~n", [?MODULE, Msg]),
    {noreply, State}.

-spec handle_info(term(), no_state) -> {noreply, no_state}.
handle_info(Info, State) ->
    logger:error("[~p] INFO: ~p~n", [?MODULE, Info]),
    {noreply, State}.

-spec terminate(term(), no_state) -> ok.
terminate(_Reason, _State) ->
    ok.

-spec code_change(term(), no_state, term()) -> {ok, no_state}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec do_login(map()) -> {ok, {pid(), player_email()}} | {error, term()}.
do_login(#{username := Username, client_pid := ProxyPid} = Request) ->
    logger:info("[~p] User ~p is attempting to login from ~p~n", [?MODULE, Username, ProxyPid]),
    case lyceum_service:check_user(Request) of
        {ok, {PlayerId, Email}} ->
            Cache = #player_cache{
                player_id = PlayerId,
                client_pid = ProxyPid,
                username = Username,
                email = Email
            },
            case lyceum_service:login_session(Cache) of
                {ok, Data, PreviousClientPid} ->
                    ok = kick_previous(PreviousClientPid),
                    start_player(Data, Email);
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% A raw send on purpose: the previous proxy may already be dead, and
%% sending to a dead pid is a harmless no-op.
-spec kick_previous(pid() | undefined) -> ok.
kick_previous(undefined) ->
    ok;
kick_previous(Pid) when is_pid(Pid) ->
    Pid ! {kick, "Logged in from another client"},
    ok.

-spec start_player(player_cache(), player_email()) ->
    {ok, {pid(), player_email()}} | {error, term()}.
start_player(Data, Email) ->
    case player_top_level_sup:start_child(Data) of
        {ok, SupPid} ->
            case supervisor:which_children(SupPid) of
                [{player, PlayerPid, worker, _}] when is_pid(PlayerPid) ->
                    logger:info(
                        "[~p] USER: ~p successfully logged at ~p!~n",
                        [?MODULE, Email, PlayerPid]
                    ),
                    {ok, {PlayerPid, Email}};
                Children ->
                    logger:error("[~p] Player worker missing: ~p~n", [?MODULE, Children]),
                    {error, "Failed to start player"}
            end;
        {error, Reason} ->
            logger:error("[~p] Failed to start player: ~p~n", [?MODULE, Reason]),
            {error, "Failed to start player"}
    end.
