%%%-------------------------------------------------------------------
%% @doc Public Server API, I don't have time to refactor this into a
%%      proper gen_server, at least not until the demo happens.
%% @end
%%%-------------------------------------------------------------------

-module(server).

% API
-export([start_link/0]).
% Internal
-export([init/1, handle_user/1]).

%% Records & Types
-include("types.hrl").

%% Macros
-define(SERVER, lyceum_server).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the process
%%--------------------------------------------------------------------
start_link() ->
    proc_lib:start_link(?MODULE, init, [self()]).

%%%===================================================================
%%% Internal functions
%%%===================================================================
init(Parent) ->
    Pid = self(),
    register(?SERVER, Pid),
    Debug = sys:debug_options([]),
    proc_lib:init_ack(Parent, {ok, Pid}),
    % Setup a DB connection and bootstrap process state
    {ok, Connection} = database:database_connect(),
    % This is a temporary solution using the built-in k/v store
    Table = ets:new(?MODULE, [named_table, private, set]),
    State =
        #state{connection = Connection,
               table = Table,
               pid = Pid},
    io:format("[~p] Starting at ~p...~n", [?SERVER, Pid]),
    main(Parent, Debug, State).

% Main Game Loop
main(Parent, Debug, State) ->
    receive
        {From, {login, Request}} ->
            login(State, From, Request);
        {list_characters, Request} ->
            list_characters(State, Request);
        {joining_map, Request} ->
            joining_map(State, Request);
        {update_character, CharacterMap} ->
            update(State, CharacterMap);
        {system, From, Request} ->
            sys:handle_system_msg(Request, From, Parent, ?MODULE, Debug, State);
        Request ->
            io:format("[~p] Sent an unsuppoted request: ~p~n", [?SERVER, Request])
    end,
    main(Parent, Debug, State).

% Initial Flow
login(State, From, #{username := Username, password := _Password} = Request) ->
    io:format("[~p] User ~p is attemptig to login from ~p~n", [?SERVER, Username, From]),
    case registry:check_user(Request, State#state.connection) of
        {ok, Email} ->
            io:format("[~p] USER: ~p successfully logged in!~n", [?SERVER, Email]),
            ets:insert(?MODULE, {Email, From}),
            io:format("[~p] <Email=~p, PID=~p> inserted...~n", [?SERVER, Email, From]),
            {ok, Connection} = database:database_connect(),
            Pid = proc_lib:spawn(?MODULE,
                                 handle_user,
                                 [#state{pid = From, connection = Connection}]),
            From ! {ok, {Pid, Email}};
        {error, Message} ->
            io:format("Unexpected Error: ~p~n", [Message]),
            From ! {error, Message}
    end.

handle_user(State) ->
    receive
        {list_characters, Request} ->
            list_characters(State, Request);
        {joining_map, Request} ->
            joining_map(State, Request);
        {update_character, CharacterMap} ->
            update(State, CharacterMap);
        exit_map ->
            exit_map(State);
        logout ->
            logout(State);
        %% TODO: We should better handle double login. Client can't know the difference between errors
        {_, {login, _}} ->
            State#state.pid ! {error, "User already logged in"};
        Request ->
            io:format("[~p] Sent an unsuppoted request: ~p~n", [State#state.pid, Request])
    end,
    handle_user(State).

% Game State Changes
list_characters(State, #{email := _Email, username := Username} = Request) ->
    io:format("[~p] Querying ~p's characters...~n", [?SERVER, Username]),
    Reply = character:player_characters(Request, State#state.connection),
    io:format("[~p] Characters: ~p~n", [?SERVER, Reply]),
    State#state.pid ! Reply.

joining_map(State,
            #{username := _Username,
              email := _Email,
              name := Name} =
                Request) ->
    Pid = State#state.pid,
    case character:activate(Request, erlang:pid_to_list(Pid), State#state.connection) of
        ok ->
            io:format("[~p] Retriving ~p's updated info...", [?SERVER, Name]),
            Result = character:player_character(Request, State#state.connection),
            io:format("Got: ~p~n", [Result]),
            Pid ! Result;
        {error, Message} ->
            io:format("Unexpected Error: ~p\n", [Message]),
            Pid ! {error, "Could not join map"}
    end.

update(State, CharacterMap) ->
    Pid = State#state.pid,
    case character:update(CharacterMap, State#state.connection) of
        ok ->
            Result =
                character:retrieve_near_players(CharacterMap,
                                                erlang:pid_to_list(Pid),
                                                State#state.connection),
            Pid ! Result;
        {error, Message} ->
            Pid ! {error, Message}
    end.

exit_map(State) ->
    Pid = State#state.pid,
    case character:deactivate(
             erlang:pid_to_list(Pid), State#state.connection)
    of
        ok ->
            Pid ! ok;
        {error, Message} ->
            io:format("Unexpected Error: ~p\n", [Message]),
            exit(2)
    end.

logout(State) ->
    Pid = State#state.pid,
    Connection = State#state.connection,
    case character:deactivate(
             erlang:pid_to_list(Pid), Connection)
    of
        ok ->
            epgsql:close(Connection),
            Pid ! ok,
            exit(normal);
        {error, Message} ->
            Pid ! {error, Message},
            exit(2)
    end.
