%%-------------------------------------------------------------------
%% @doc
%% A Player's Finite State Machine, each user has its own FSM that
%% triggers changes into the World (and other players).
%%
%% States: logged_in, in_game, logged_out
%% @end
%%%-------------------------------------------------------------------
-module(player).

-behaviour(gen_statem).

%% API
-export([start_link/1]).
%% gen_statem callbacks
-export([callback_mode/0, init/1, terminate/3, code_change/4]).
%% State functions
-export([logged_in/3, in_game/3]).

-include("player_state.hrl").
-include("player_fsm_state.hrl").

%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% Starts a player's FSM
%% @end
%%--------------------------------------------------------------------
-spec start_link(Cache) -> Result when
    Cache :: player_cache(),
    Result :: gen_statem:start_ret().
start_link(Cache) ->
    logger:debug("[~p] GEN_STATEM ARGS ~p~n", [?MODULE, Cache]),
    PlayerId = Cache#player_cache.player_id,
    State = to_state(Cache),
    logger:debug("[~p] GEN_STATEM ID = ~p WITH STATE = ~p~n", [?MODULE, PlayerId, State]),
    gen_statem:start_link(?MODULE, State, []).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================
-doc """
Defines callback mode, we default to state_functions instead of event
handlers.
""".
callback_mode() ->
    state_functions.

-doc """
Initializes the state machine
""".
-spec init(State) -> Return when
    State :: player_state(),
    Return :: gen_statem:init_result(player_fsm_state()).
init(State) ->
    %% The login reply travels back through player_session; this
    %% process never talks to the client directly, only to its proxy.
    %% The pg membership replaces the old {global, PlayerId} name: it
    %% is observability plus a kick fallback, never a routing handle.
    ok = lyceum_cluster:join({player, State#player_state.player_id}),
    _ = monitor(process, State#player_state.client_pid),
    {ok, logged_in, State}.

%%%===================================================================
%%% State Functions
%%%===================================================================
-doc """
State function for when player is logged in, but hasn't selected a
character nor has joined any map.
""".
-spec logged_in(EventType, term(), State) -> Return when
    EventType :: gen_statem:event_type(),
    State :: player_state(),
    Return :: gen_statem:event_handler_result(atom()).
logged_in(info, {list_characters, Request}, State) ->
    list_characters(State, Request),
    {keep_state, State};
logged_in(
    info,
    {joining_map,
        #{
            username := Username,
            email := Email,
            name := Name
        } =
            Request},
    State
) ->
    case joining_map(State, Request) of
        ok ->
            Data =
                #player_data{
                    email = Email,
                    username = Username,
                    character_name = Name
                },
            NewState = State#player_state{data = Data},
            {next_state, in_game, NewState};
        {error, Reason} ->
            logger:error("[~p] ERROR WHILE joining_map: ~p", [?MODULE, Reason]),
            {keep_state, State}
    end;
logged_in(info, {update_character, Request}, State) ->
    update(State, Request),
    {keep_state, State};
logged_in(info, logout, State) ->
    logout(State),
    {stop, normal, State};
logged_in(EventType, Event, State) ->
    handle_common_events(EventType, Event, State, logged_in).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% State function for when player is ready to play the game.
%% @end
%%--------------------------------------------------------------------
-spec in_game(EventType, term(), State) -> Return when
    EventType :: gen_statem:event_type(),
    State :: player_state(),
    Return :: gen_statem:event_handler_result(atom()).
in_game(info, {update_character, Request}, State) ->
    update(State, Request),
    {keep_state, State};
in_game(info, {harvest_resource, Request}, State) ->
    harvest_resource(State, Request),
    {keep_state, State};
in_game(info, exit_map, State) ->
    case exit_map(State) of
        ok ->
            {next_state, logged_in, State};
        {error, _} ->
            {stop, {shutdown, exit_map_failed}, State}
    end;
in_game(info, {list_characters, Request}, State) ->
    list_characters(State, Request),
    {keep_state, State};
in_game(EventType, Event, State) ->
    handle_common_events(EventType, Event, State, in_game).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This is called by a gen_statem when it is about to terminate.
%% @end
%%--------------------------------------------------------------------
-spec terminate(term(), atom(), State) -> Return when
    State :: player_state(),
    Return :: ok.
terminate(Reason, StateName, _State) ->
    logger:info("[~p] Termination in state ~p: ~p~n", [?MODULE, StateName, Reason]),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
-spec code_change(term(), atom(), State, term()) -> Return when
    State :: player_state(),
    Return :: {ok, atom(), State}.
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handle events common to all states
%% @end
%%--------------------------------------------------------------------
-spec handle_common_events(EventType, term(), State, atom()) -> Return when
    EventType :: gen_statem:event_type(),
    State :: player_state(),
    Return :: gen_statem:event_handler_result(atom()).
handle_common_events(
    info,
    {'DOWN', _Ref, process, Pid, Reason},
    #player_state{client_pid = Pid} = State,
    StateName
) ->
    %% The proxy is gone: client disconnect, kick by a newer login, or
    %% a lost frontend. Deactivate the character but leave the session
    %% cache row alone, after a kick that row already belongs to the
    %% new session, and a stale row is healed by the next login.
    logger:info(
        "[~p] Proxy ~p went down in state ~p: ~p~n",
        [?MODULE, Pid, StateName, Reason]
    ),
    _ = lyceum_service:exit_map(character_id(State)),
    {stop, normal, State};
handle_common_events(EventType, Info, State, StateName) ->
    logger:warning(
        "[~p] UNHANDLED EVENT ~p IN STATE ~p: ~p~n",
        [?MODULE, EventType, StateName, Info]
    ),
    {keep_state, State}.

-spec to_state(Cache) -> State when
    Cache :: player_cache(),
    State :: player_state().
to_state(Cache) ->
    PlayerId = Cache#player_cache.player_id,
    Username = Cache#player_cache.username,
    Email = Cache#player_cache.email,
    ClientPid = Cache#player_cache.client_pid,
    Data = #player_data{username = Username, email = Email},
    #player_state{
        client_pid = ClientPid,
        player_id = PlayerId,
        data = Data
    }.

-spec list_characters(State, PlayerMap) -> Return when
    State :: player_state(),
    Email :: player_email(),
    Username :: player_name(),
    PlayerMap :: #{email := Email, username := Username},
    Return :: ok.
list_characters(State, #{email := _, username := Username} = Request) ->
    logger:info("[~p] Querying ~p's characters...~n", [?MODULE, Username]),
    Reply = lyceum_service:list_characters(Request),
    logger:info("[~p] Characters: ~p~n", [?MODULE, Reply]),
    reply_to_client(State, Reply).

-spec joining_map(State, Map) -> Return when
    State :: player_state(),
    Name :: player_name(),
    MapName :: string(),
    Map :: #{name := Name, map_name := MapName},
    Reason :: string(),
    Return :: ok | {error, Reason}.
joining_map(State, #{name := Name, map_name := _} = Request) ->
    logger:info("[~p] ~p is joining a map...", [?MODULE, Name]),
    case lyceum_service:join_map(Request) of
        {ok, _} = Result ->
            reply_to_client(State, Result);
        {error, Message} ->
            logger:error("Failed to Join Map: ~p~n", [Message]),
            ok = reply_to_client(State, {error, "Could not join map"}),
            {error, Message}
    end.

atom_to_upperstring(Atom) ->
    string:uppercase(atom_to_list(Atom)).

-spec harvest_resource(player_state(), map()) -> ok.
harvest_resource(State, Request) ->
    Result =
        lyceum_service:harvest_resource(
            maps:update_with(kind, fun atom_to_upperstring/1, Request)
        ),
    logger:info("Harvest Result: ~p\n", [Result]),
    reply_to_client(State, Result).

-spec update(player_state(), map()) -> ok.
update(State, CharacterMap) ->
    case lyceum_service:update_character(CharacterMap) of
        {ok, _} = Result ->
            reply_to_client(State, Result);
        {error, Message} ->
            logger:error("Failed to Update: ~p~n", [Message]),
            reply_to_client(State, {error, Message})
    end.

-spec exit_map(player_state()) -> ok | {error, term()}.
exit_map(State) ->
    case lyceum_service:exit_map(character_id(State)) of
        ok ->
            reply_to_client(State, ok);
        {error, Message} ->
            logger:error("[~p] Failed to ExitMap: ~p~n", [?MODULE, Message]),
            ok = reply_to_client(State, {error, Message}),
            {error, Message}
    end.

-spec logout(State) -> Return when
    State :: player_state(),
    Return :: ok.
logout(State) ->
    Request = maps:put(player_id, State#player_state.player_id, character_id(State)),
    case lyceum_service:logout_player(Request) of
        ok ->
            reply_to_client(State, ok);
        {error, Message} ->
            reply_to_client(State, {error, Message})
    end.

%% Client-bound messages are tagged so the proxy can tell them apart
%% from client-originated traffic it forwards the other way.
-spec reply_to_client(player_state(), term()) -> ok.
reply_to_client(State, Msg) ->
    State#player_state.client_pid ! {reply, Msg},
    ok.

-spec character_id(player_state()) -> #{name := _, email := _, username := _}.
character_id(State) ->
    #{
        name => State#player_state.data#player_data.character_name,
        email => State#player_state.data#player_data.email,
        username => State#player_state.data#player_data.username
    }.
