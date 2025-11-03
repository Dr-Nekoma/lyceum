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

-compile({parse_transform, do}).

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
    {ok, Conn} = database:connect(),
    PlayerId = Cache#player_cache.player_id,
    State = to_state(Conn, Cache),
    logger:debug("[~p] GEN_STATEM ID = ~p WITH STATE = ~p~n", [?MODULE, PlayerId, State]),
    gen_statem:start_link({global, PlayerId}, ?MODULE, State, []).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Define callback mode
%% https://www.erlang.org/doc/apps/stdlib/gen_statem.html#t:callback_mode/0
%% @end
%%--------------------------------------------------------------------
callback_mode() ->
    state_functions.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the state machine
%% @end
%%--------------------------------------------------------------------
-spec init(State) -> Return when
    State :: player_state(),
    Return :: gen_statem:init_result(player_fsm_state()).
init(State) ->
    ClientPid = State#player_state.client_pid,
    PlayerData = State#player_state.data,
    PlayerEmail = PlayerData#player_data.email,
    PlayerPid = self(),
    Reply = {ok, {PlayerPid, PlayerEmail}},
    ClientPid ! Reply,
    {ok, logged_in, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% State function for when player is logged in, but hasn't selected a
%% character nor has joined any map.
%% @end
%%--------------------------------------------------------------------
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
handle_common_events(EventType, Info, State, StateName) ->
    logger:warning(
        "[~p] UNHANDLED EVENT ~p IN STATE ~p: ~p~n",
        [?MODULE, EventType, StateName, Info]
    ),
    {keep_state, State}.

-spec to_state(Conn, Cache) -> State when
    Conn :: epgsql:connection(),
    Cache :: player_cache(),
    State :: player_state().
to_state(Conn, Cache) ->
    PlayerId = Cache#player_cache.player_id,
    Username = Cache#player_cache.username,
    Email = Cache#player_cache.email,
    ClientPid = Cache#player_cache.client_pid,
    Data = #player_data{username = Username, email = Email},
    #player_state{
        connection = Conn,
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
    Conn = State#player_state.connection,
    Reply = character:player_characters(Request, Conn),
    logger:info("[~p] Characters: ~p~n", [?MODULE, Reply]),
    State#player_state.client_pid ! Reply,
    ok.

-spec joining_map(State, Map) -> Return when
    State :: player_state(),
    Name :: player_name(),
    MapName :: string(),
    Map :: #{name := Name, map_name := MapName},
    Reason :: string(),
    Return :: ok | {error, Reason}.
joining_map(State, #{name := Name, map_name := MapName} = Request) ->
    Pid = State#player_state.client_pid,
    Connection = State#player_state.connection,
    case character:activate(Request, Connection) of
        ok ->
            logger:info("[~p] Retrieving ~p's updated info...", [?MODULE, Name]),
            Result =
                do([
                    error_m
                 || Character <- character:player_character(Request, Connection),
                    Map <- map:get_map(MapName, Connection),
                    logger:info("Retrieving ~p's map...", [MapName]),
                    return(#{character => Character, map => Map})
                ]),
            Pid ! Result,
            ok;
        {error, Message} ->
            logger:error("Failed to Join Map: ~p~n", [Message]),
            Pid ! {error, "Could not join map"},
            {error, Message}
    end.

atom_to_upperstring(Atom) ->
    string:uppercase(atom_to_list(Atom)).

-spec harvest_resource(player_state(), map()) -> ok.
harvest_resource(State, Request) ->
    Pid = State#player_state.client_pid,
    Connection = State#player_state.connection,
    Result =
        character:harvest_resource(
            maps:update_with(kind, fun atom_to_upperstring/1, Request), Connection
        ),
    logger:info("Harvest Result: ~p\n", [Result]),
    Pid ! Result.

-spec update(player_state(), map()) -> term().
update(State, CharacterMap) ->
    Pid = State#player_state.client_pid,
    case character:update(CharacterMap, State#player_state.connection) of
        ok ->
            Result = character:retrieve_near_players(CharacterMap, State#player_state.connection),
            Pid ! Result;
        {error, Message} ->
            logger:error("Failed to Update: ~p~n", [Message]),
            Pid ! {error, Message}
    end.

-spec exit_map(player_state()) -> ok | {error, term()}.
exit_map(State) ->
    Pid = State#player_state.client_pid,
    Name = State#player_state.data#player_data.character_name,
    Email = State#player_state.data#player_data.email,
    Username = State#player_state.data#player_data.username,
    case character:deactivate(Name, Email, Username, State#player_state.connection) of
        ok ->
            Pid ! ok,
            ok;
        {error, Message} ->
            logger:error("[~p] Failed to ExitMap: ~p~n", [?MODULE, Message]),
            Pid ! {error, Message},
            {error, Message}
    end.

-spec logout(State) -> Exit when
    State :: player_state(),
    Exit :: no_return().
logout(State) ->
    Pid = State#player_state.client_pid,
    Name = State#player_state.data#player_data.character_name,
    Email = State#player_state.data#player_data.email,
    Username = State#player_state.data#player_data.username,
    Connection = State#player_state.connection,
    case character:deactivate(Name, Email, Username, Connection) of
        ok ->
            cache:logout(State#player_state.player_id),
            Pid ! ok,
            ok;
        {error, Message} ->
            Pid ! {error, Message},
            ok
    end.
