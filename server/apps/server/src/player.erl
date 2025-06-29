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

-include("user_state.hrl").

-compile({parse_transform, do}).

-dialyzer({nowarn_function,
           [exit_map/1, logout/1, joining_map/2, update/2, harvest_resource/2]}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
-spec start_link(term()) -> gen_server:start_ret().
start_link(Args) ->
    logger:debug("GEN_SERVER ARGS ~p~n", [Args]),
    {Name, State} = Args,
    logger:debug("[~p] GEN_SERVER NAME = ~p~nGEN_SERVER STATE = ~p~n", [?MODULE, Name, State]),
    gen_server:start_link({global, Name}, ?MODULE, State, []).

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
-spec init(user_state()) -> {ok, user_state()}.
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
-spec handle_call(term(), gen_server:from(), user_state()) -> Return
    when Return ::
             {reply, term(), user_state()} |
             {reply, term(), user_state(), timeout()} |
             {noreply, user_state()} |
             {noreply, user_state(), timeout()} |
             {stop, term(), term(), user_state()} |
             {stop, term(), user_state()}.
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
-spec handle_cast(term(), user_state()) -> Return
    when Return ::
             {noreply, user_state()} |
             {noreply, user_state(), timeout()} |
             {stop, term(), user_state()}.
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
-spec handle_info(term(), user_state()) -> Return
    when Return ::
             {noreply, user_state()} | {noreply, user_state()} | {stop, term(), user_state()}.
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
    NewState =
        #user_state{pid = OldState#user_state.pid,
                    connection = OldState#user_state.connection,
                    email = Email,
                    username = Username,
                    name = Name},
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
    State#user_state.pid ! {error, "User already logged in"},
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
-spec terminate(term(), user_state()) -> ok.
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
-spec code_change(term(), user_state(), term()) -> Return
    when Return :: {ok, user_state()} | {error, term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec list_characters(user_state(), map()) -> term().
list_characters(State, #{email := _Email, username := Username} = Request) ->
    logger:info("[~p] Querying ~p's characters...~n", [?MODULE, Username]),
    Reply = character:player_characters(Request, State#user_state.connection),
    logger:info("[~p] Characters: ~p~n", [?MODULE, Reply]),
    State#user_state.pid ! Reply.

-spec joining_map(user_state(), map()) -> ok.
joining_map(State, #{name := Name, map_name := MapName} = Request) ->
    Pid = State#user_state.pid,
    Connection = State#user_state.connection,
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

atom_to_upperstring(Atom) -> string:uppercase(atom_to_list(Atom)).

-spec harvest_resource(user_state(), map()) -> ok.
harvest_resource(State, Request) ->
    Pid = State#user_state.pid,
    Connection = State#user_state.connection,   
    Result = character:harvest_resource(maps:update_with(kind, fun atom_to_upperstring/1, Request), Connection),
    logger:info("Harvest Result: ~p\n", [Result]),
    Pid ! Result.

-spec update(user_state(), map()) -> term().
update(State, CharacterMap) ->
    Pid = State#user_state.pid,
    case character:update(CharacterMap, State#user_state.connection) of
        ok ->
            Result = character:retrieve_near_players(CharacterMap, State#user_state.connection),
            Pid ! Result;
        {error, Message} ->
            logger:error("Failed to Update: ~p~n", [Message]),
            Pid ! {error, Message}
    end.

-spec exit_map(user_state()) -> term() | no_return().
exit_map(State) ->
    Pid = State#user_state.pid,
    case character:deactivate(State#user_state.name,
                              State#user_state.email,
                              State#user_state.username,
                              State#user_state.connection)
    of
        ok ->
            Pid ! ok;
        {error, Message} ->
            logger:error("Failed to ExitMap: ~p~n", [Message]),
            exit(2)
    end.

-spec logout(user_state()) -> no_return().
logout(State) ->
    Pid = State#user_state.pid,
    Connection = State#user_state.connection,
    case character:deactivate(State#user_state.name,
                              State#user_state.email,
                              State#user_state.username,
                              Connection)
    of
        ok ->
            epgsql:close(Connection),
            gen_server:cast(lyceum_server, {logout, State#user_state.email}),
            Pid ! ok,
            exit(normal);
        {error, Message} ->
            Pid ! {error, Message},
            exit(2)
    end.
