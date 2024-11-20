%%%-------------------------------------------------------------------
%% @doc Player Server, each user is its own process.
%% @end
%%%-------------------------------------------------------------------
-module(player).

%% API
-export([start_link/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-include("types.hrl").

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
start_link(Args) ->
    io:format("GEN_SERVER ARGS ~p~n", [Args]),
    {Name, State} = Args,
    io:format("[~p] GEN_SERVER NAME = ~p~nGEN_SERVER STATE = ~p~n", [?MODULE, Name, State]),
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
handle_info({list_characters, Request}, State) ->
    list_characters(State, Request),
    {noreply, State};
handle_info({joining_map,
             #{username := Username,
               email := Email,
               name := Name} =
                 Request},
            OldState) ->
    joining_map(OldState, Request),
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
handle_info(exit_map, State) ->
    exit_map(State),
    {noreply, State};
handle_info(logout, State) ->
    logout(State),
    {noreply, State};
handle_info({_, {login, _}}, State) ->
    State#user_state.pid ! {error, "User already logged in"};
handle_info(Info, State) ->
    io:format("[~p] INFO: ~p~n", [?MODULE, Info]),
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
terminate(Reason, _State) ->
    io:format("[~p] Termination: ~p~n", [?MODULE, Reason]),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
list_characters(State, #{email := _Email, username := Username} = Request) ->
    io:format("[~p] Querying ~p's characters...~n", [?MODULE, Username]),
    Reply = character:player_characters(Request, State#user_state.connection),
    io:format("[~p] Characters: ~p~n", [?MODULE, Reply]),
    State#user_state.pid ! Reply.

joining_map(State, #{name := Name, map_name := MapName} = Request) ->
    Pid = State#user_state.pid,
    Connection = State#user_state.connection,
    case character:activate(Request, Connection) of
        ok ->
            io:format("[~p] Retriving ~p's updated info...", [?MODULE, Name]),
            Result =
                database_utils:psql_bind(
                    character:player_character(Request, Connection),
                    [fun(Character) ->
                        io:format("Retriving ~p's map...", [MapName]),
                        database_utils:psql_bind(
                            map:get_map(MapName, Connection),
                            [fun(Map) -> {ok, #{character => Character, map => Map}} end])
                     end]),
            Pid ! Result;
        {error, Message} ->
            io:format("Failed to Join Map: ~p~n", [Message]),
            Pid ! {error, "Could not join map"}
    end.

update(State, CharacterMap) ->
    Pid = State#user_state.pid,
    case character:update(CharacterMap, State#user_state.connection) of
        ok ->
            Result = character:retrieve_near_players(CharacterMap, State#user_state.connection),
            Pid ! Result;
        {error, Message} ->
            io:format("Failed to Update: ~p\n", [Message]),
            Pid ! {error, Message}
    end.

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
            io:format("Failed to ExitMap: ~p~n", [Message]),
            exit(2)
    end.

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
