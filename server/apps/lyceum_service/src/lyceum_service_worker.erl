-module(lyceum_service_worker).
-moduledoc """
Executes service-layer operations on a node that hosts the pools.

Workers join the `service` pg group in `init/1`; callers find one
through `lyceum_service`. Every operation runs in the worker process
on purpose: pgo binds queries and transactions to the calling process,
so running them here guarantees they execute on the node that owns
`lyceum_pool`/`auth_pool`, whatever node the original caller is on.
The session store in `cache` runs here for the same reason: it is a
plain module, so its transaction belongs to this process.
""".

-behaviour(gen_server).

-export([start_link/0]).
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-include("player_state.hrl").

-compile({parse_transform, do}).

-define(POOL, lyceum_pool).
-define(AUTH_POOL, auth_pool).

-spec start_link() -> gen_server:start_ret().
start_link() ->
    gen_server:start_link(?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec init([]) -> {ok, no_state}.
init([]) ->
    ok = lyceum_cluster:join(service),
    {ok, no_state}.

-spec handle_call(term(), gen_server:from(), no_state) -> {reply, term(), no_state}.
handle_call({check_user, Request}, _From, State) ->
    {reply, registry:check_user(Request, ?AUTH_POOL), State};
handle_call({login_session, Cache}, {Caller, _Tag}, State) ->
    %% The caller is `player_session` on a logic node, so its node is
    %% the one that will own the player FSM. Recording it here is what
    %% lets the reaper close the session if that node dies: the service
    %% layer can see logic nodes, never the frontend node the client
    %% pid lives on.
    {reply, cache:login(Cache, node(Caller)), State};
handle_call({logout_session, PlayerId}, _From, State) ->
    {reply, cache:logout(PlayerId), State};
handle_call({list_characters, Request}, _From, State) ->
    {reply, character:player_characters(Request, ?POOL), State};
handle_call({join_map, Request}, _From, State) ->
    {reply, join_map(Request), State};
handle_call({update_character, Request}, _From, State) ->
    {reply, update_character(Request), State};
handle_call({harvest_resource, Request}, _From, State) ->
    {reply, character:harvest_resource(Request, ?POOL), State};
handle_call({exit_map, Request}, _From, State) ->
    {reply, exit_map(Request), State};
handle_call({logout_player, Request}, _From, State) ->
    {reply, logout_player(Request), State};
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
%%% Operations
%%%===================================================================

-spec join_map(map()) -> {ok, map()} | {error, term()}.
join_map(#{map_name := MapName} = Request) ->
    case character:activate(Request, ?POOL) of
        ok ->
            do([
                error_m
             || Character <- character:player_character(Request, ?POOL),
                Map <- map:get_map(MapName, ?POOL),
                return(#{character => Character, map => Map})
            ]);
        {error, _} = Error ->
            Error
    end.

-spec update_character(map()) -> {ok, [map()]} | {error, term()}.
update_character(Request) ->
    case character:update(Request, ?POOL) of
        ok ->
            character:retrieve_near_players(Request, ?POOL);
        {error, _} = Error ->
            Error
    end.

-spec exit_map(map()) -> ok | {error, term()}.
exit_map(#{name := Name, email := Email, username := Username}) ->
    character:deactivate(Name, Email, Username, ?POOL).

-spec logout_player(map()) -> ok | {error, term()}.
logout_player(#{
    name := Name,
    email := Email,
    username := Username,
    player_id := PlayerId
}) ->
    case character:deactivate(Name, Email, Username, ?POOL) of
        ok ->
            cache:logout(PlayerId);
        {error, _} = Error ->
            Error
    end.
