%%%-------------------------------------------------------------------
%% @doc server public API
%% @end
%%%-------------------------------------------------------------------

-module(server).

-behaviour(gen_server).

%% API
-export([start_link/0, login/0, stop/0]).

%% Internal
%-export([start/2, stop/1, main/1, handle_master/1, handle_user/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, terminate/2, code_change/3]).

%% Records
-record(state, {connection}).

%-----------%
%%%% API %%%%
%-----------%

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

login() ->
    gen_server:call(?MODULE, login).

stop() ->
    gen_server:cast(?MODULE, stop).

%----------------%
%%%% Internal %%%%
%----------------%
% TODO: We should not just keep passing Connections here, this is dangerous for interactive stuff
handle_user(#{user_pid := UserPid, connection := Connection} = State) ->
    receive
        {list_characters, #{username := Username, email := Email} = Something} ->
            io:format("Querying user's characters...\n"),
            io:format("Map: ~p\n", [Something]),
            Characters = character:player_characters(Username, Email, Connection),
            io:format("Characters: ~p\n", [Characters]),
            UserPid ! {ok, Characters};
        {create_character, Character_Map} ->
            io:format("This character logged"),
            character:create(Character_Map, Connection),
            UserPid ! ok;
        {joining_map, Character_Map} ->
            io:format("User is about to join a world!\n"),
            case character:activate(Character_Map, erlang:pid_to_list(UserPid), Connection) of
                ok ->
                    io:format("Everything went ok with User: ~p!\n", [UserPid]),
                    UserPid ! ok;
                already_active ->
                    io:format("Double joining\n"),
                    exit(1);
                {error, Message} ->
                    io:format("Unexpected Error: ~p\n", [Message]),
                    exit(2)
            end;
        exit_map ->
            case character:deactivate(
                     erlang:pid_to_list(UserPid), Connection)
            of
                ok ->
                    UserPid ! ok;
                already_inactive ->
                    io:format("Double leaving\n"),
                    exit(1);
                {error, Message} ->
                    io:format("Unexpected Error: ~p\n", [Message]),
                    exit(2)
            end;
        disconnect ->
            character:deactivate(
                erlang:pid_to_list(UserPid), Connection),
            epgsql:close(Connection),
            UserPid ! ok,
            exit(0);
        {update_character, Character_Map} ->
            io:format("Character will be updated. User: ~p\n", [UserPid]),
            character:update(Character_Map, Connection),
            io:format("Retrieving nearby characters...\n"),
            Players =
                character:retrieve_near_players(Character_Map,
                                                erlang:pid_to_list(UserPid),
                                                Connection),
            io:format("Everything went ok!\n"),
            UserPid ! {ok, Players}
    end,
    handle_user(State).

handle_master(Connection) ->
    receive
        {Pid,
         {register,
          #{username := Username,
            email := Email,
            password := Password}}} ->
            io:format("This user now exists: ~p", [Username]),
            registry:insert_user(#{username => Username,
                                   password => Password,
                                   email => Email},
                                 Connection),
            Response = "I registered " ++ Username,
            Pid ! {self(), Response};
        {Pid, {login, #{username := Username, password := Password}}} ->
            io:format("This user logged: ~p\n", [Username]),
            Email = registry:check_user(#{username => Username, password => Password}, Connection),
            NewPid =
                spawn(?MODULE,
                      handle_user,
                      [#{user_pid => Pid, connection => database:database_connect()}]),
            io:format("User's email: ~p\n", [Email]),
            Pid ! {ok, {NewPid, Email}};
        {Pid, Value} ->
            io:format("Yo, we received something ~p ", [Value]),
            Pid ! {self(), "Yo bruh, I got you xD"}
    end,
    handle_master(Connection).

%------------------%
%%%% Gen Server %%%%
%------------------%
init([]) ->
    case database:database_connect() of
        {ok, Conn} ->
            database:migrate(Conn),
            Pid = spawn(?MODULE, handle_master, [Conn]),
            erlang:register(lyceum_server, Pid),
            {ok, Pid};
        {error, _Reason} ->
            %% If connection fails, crash to trigger restart
            {stop, failed_to_connect}
    end.

handle_call(login, _From, State) ->
    {reply, ok, State}.

handle_cast(stop, State) ->
    %% Gracefully close connection if stopping
    {ok, _} = epgsql:close(State#state.connection),
    {stop, normal, State}.

terminate(_Reason, State) ->
    case State#state.connection of
        undefined ->
            ok;
        _ ->
            epgsql:close(State#state.connection)
    end.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
