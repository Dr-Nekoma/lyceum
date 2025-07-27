%%%-------------------------------------------------------------------
%% @doc Player Server, each user is its own process.
%% @end
%%%-------------------------------------------------------------------
-module(player).

-behaviour(gen_server).

%% API
-export([start_link/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-include("player_state.hrl").

-compile({parse_transform, do}).

-dialyzer({nowarn_function,
           [exit_map/1, logout/1, joining_map/2, update/2, harvest_resource/2]}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts a player's server
%% @end
%%--------------------------------------------------------------------
-spec start_link(Cache) -> Result when 
    Cache :: player_cache(),
    Result :: gen_server:start_ret().
start_link(Cache) ->
    logger:debug("GEN_SERVER ARGS ~p~n", [Cache]),
    {ok, Conn} = database:connect(),
    PlayerId = Cache#player_cache.player_id,
    State = cache_to_state(Conn, Cache),
    logger:debug("[~p] GEN_SERVER NAME = ~p~nGEN_SERVER STATE = ~p~n",
                 [?MODULE, PlayerId, State]),
    gen_server:start_link({global, PlayerId}, ?MODULE, State, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec init(player_state()) -> {ok, player_state()}.
init(State) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec handle_call(term(), gen_server:from(), player_state()) -> Return
    when Return ::
             {reply, term(), player_state()} |
             {reply, term(), player_state(), timeout()} |
             {noreply, player_state()} |
             {noreply, player_state(), timeout()} |
             {stop, term(), term(), player_state()} |
             {stop, term(), player_state()}.
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(term(), player_state()) -> Return
    when Return ::
             {noreply, player_state()} |
             {noreply, player_state(), timeout()} |
             {stop, term(), player_state()}.
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec handle_info(term(), player_state()) -> Return
    when Return ::
             {noreply, player_state()} | {noreply, player_state()} | {stop, term(), player_state()}.
handle_info({list_characters, Request}, State) ->
    list_characters(State, Request),
    {noreply, State};
handle_info({joining_map,
             #{username := Username,
               email := Email,
               name := Name} =
                 Request},
            OldState) ->
    ok = joining_map(OldState, Request),
    Data =
        #player_data{email = Email,
                     username = Username,
                     character_name = Name},
    NewState = OldState#player_state{data = Data},
    {noreply, NewState};
handle_info({update_character, Request}, State) ->
    update(State, Request),
    {noreply, State};
handle_info({harvest_resource, Request}, State) ->
    harvest_resource(State, Request),
    {noreply, State};
handle_info(exit_map, State) ->
    exit_map(State),
    {noreply, State};
handle_info(logout, State) ->
    logout(State),
    {noreply, State};
handle_info({_, {login, _}}, State) ->
    State#player_state.client_pid ! {error, "User already logged in"},
    {noreply, State};
handle_info(Info, State) ->
    logger:warning("[~p] HANDLE_INFO: ~p~n", [?MODULE, Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec terminate(term(), player_state()) -> ok.
terminate(Reason, _State) ->
    logger:info("[~p] Termination: ~p~n", [?MODULE, Reason]),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec code_change(term(), player_state(), term()) -> Return
    when Return :: {ok, player_state()} | {error, term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec cache_to_state(Conn, Cache) -> State
    when
      Conn :: epgsql:connection(),
      Cache :: player_cache(),
      State :: player_state().
cache_to_state(Conn, Cache) ->
    PlayerId = Cache#player_cache.player_id,
    Username = Cache#player_cache.username,
    Email = Cache#player_cache.email,
    ClientPid = Cache#player_cache.client_pid,
    Data = #player_data{username = Username, email = Email},
    #player_state{connection = Conn, client_pid = ClientPid, player_id = PlayerId, data = Data}.

-spec list_characters(player_state(), map()) -> term().
list_characters(State, #{email := _, username := Username} = Request) ->
    logger:info("[~p] Querying ~p's characters...~n", [?MODULE, Username]),
    Conn = State#player_state.connection,
    Reply = character:player_characters(Request, Conn),
    logger:info("[~p] Characters: ~p~n", [?MODULE, Reply]),
    State#player_state.client_pid ! Reply.

-spec joining_map(player_state(), map()) -> ok.
joining_map(State, #{name := Name, map_name := MapName} = Request) ->
    Pid = State#player_state.client_pid,
    Connection = State#player_state.connection,
    case character:activate(Request, Connection) of
        ok ->
            logger:info("[~p] Retriving ~p's updated info...", [?MODULE, Name]),
            Result =
                do([error_m
                    || Character <- character:player_character(Request, Connection),
                       Map <- map:get_map(MapName, Connection),
                       logger:info("Retriving ~p's map...", [MapName]),
                       return(#{character => Character, map => Map})]),
            Pid ! Result;
        {error, Message} ->
            logger:error("Failed to Join Map: ~p~n", [Message]),
            Pid ! {error, "Could not join map"}
    end,
    ok.

atom_to_upperstring(Atom) ->
    string:uppercase(atom_to_list(Atom)).

-spec harvest_resource(player_state(), map()) -> ok.
harvest_resource(State, Request) ->
    Pid = State#player_state.client_pid,
    Connection = State#player_state.connection,
    Result =
        character:harvest_resource(
            maps:update_with(kind, fun atom_to_upperstring/1, Request), Connection),
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

-spec exit_map(player_state()) -> term() | no_return().
exit_map(State) ->
    Pid = State#player_state.client_pid,
    Name = State#player_state.data#player_data.character_name,
    Email = State#player_state.data#player_data.email,
    Username = State#player_state.data#player_data.username,
    case character:deactivate(Name, Email, Username, State#player_state.connection) of
        ok ->
            Pid ! ok;
        {error, Message} ->
            logger:error("Failed to ExitMap: ~p~n", [Message]),
            exit(2)
    end.

-spec logout(player_state()) -> no_return().
logout(State) ->
    Pid = State#player_state.client_pid,
    Name = State#player_state.data#player_data.character_name,
    Email = State#player_state.data#player_data.email,
    Username = State#player_state.data#player_data.username,
    Connection = State#player_state.connection,
    case character:deactivate(Name, Email, Username, Connection) of
        ok ->
            gen_server:cast(mnesia_storage, {logout, State#player_state.player_id}),
            Pid ! ok,
            exit(normal);
        {error, Message} ->
            Pid ! {error, Message},
            exit(2)
    end.
