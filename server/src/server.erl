%%%-------------------------------------------------------------------
%% @doc server public API
%% @end
%%%-------------------------------------------------------------------

-module(server).

-behaviour(application).

-export([start/2, stop/1, main/1, handle_master/1, handle_user/1]).

%% TODO: We shall remove the cookie given that this is a public game, lmao
start(_, _) ->
    Connection = database:database_connect(),
    %% database:migrate(Connection),
    Pid = spawn(?MODULE, handle_master, [Connection]),
    erlang:register(lyceum_server, Pid),
    {ok, Pid}.

%% stream_user_data(#{user_pid := UserPid, connection := Connection} = State) ->
%%     {ok, Data} = epgsql:set_notice_receiver(Connection, self()),
%%     io:format("Data: ~p\n", [Data]),
%%     %% UserPid ! {ok, Data},
%%     stream_user_data(State).

%% timeout_user(State) ->

% TODO: We should not just keep passing Connections here, this is dangerous for interactive stuff
handle_user(#{user_pid := UserPid, connection := Connection} = State) ->
    receive
        {list_characters, #{username := Username, email := Email} = Something} ->
            io:format("Querying user's characters...\n"),
            io:format("Map: ~p\n", [Something]),
            Result = character:player_characters(Username, Email, Connection),
            io:format("Characters: ~p\n", [Result]),
            UserPid ! Result;
        {create_character, Character_Map} ->
            io:format("This character logged"),
            Result = character:create(Character_Map, Connection),
            UserPid ! Result;
        {joining_map, Character_Map} ->
            io:format("User is about to join a world!\n"),
            case character:activate(Character_Map, erlang:pid_to_list(UserPid), Connection) of
                ok ->
                    io:format("Everything went ok with User: ~p!\n", [UserPid]),
                    UserPid ! ok;
                {error, Message} ->
                    io:format("Unexpected Error: ~p\n", [Message]),
                    UserPid ! {error, "Could not join map"}
            end;
        exit_map ->
            case
                character:deactivate(
                    erlang:pid_to_list(UserPid), Connection
                )
            of
                ok ->
                    UserPid ! ok;
                {error, Message} ->
                    io:format("Unexpected Error: ~p\n", [Message]),
                    exit(2)
            end;
        logout ->
            epgsql:close(Connection),
            UserPid ! ok,
            exit(0);
        %% TODO: We should better handle double login. Client can't know the difference between errors
        {_, {login, _}} ->
            UserPid ! {error, "User already logged in"};
        {update_character, Character_Map} ->
            io:format("Character will be updated. User: ~p\n", [UserPid]),
            case character:update(Character_Map, Connection) of
                ok ->
                    io:format("Retrieving nearby characters...\n"),
                    Result =
                        character:retrieve_near_players(
                            Character_Map,
                            erlang:pid_to_list(UserPid),
                            Connection
                        ),
                    io:format("Everything went ok!\n"),
                    UserPid ! Result;
                {error, Message} ->
                    io:format("Unexpected Error: ~p\n", [Message]),
                    UserPid ! {error, Message}
            end
    end,
    handle_user(State).

handle_master(Connection) ->
    receive
        {Pid,
            {register, #{
                username := Username,
                email := Email,
                password := Password
            }}} ->
            io:format("This user now exists: ~p", [Username]),
            case
                registry:insert_user(
                    #{
                        username => Username,
                        password => Password,
                        email => Email
                    },
                    Connection
                )
            of
                ok ->
                    Response = "I registered " ++ Username,
                    Pid ! {self(), Response};
                {error, Message} ->
                    io:format("Unexpected Error: ~p\n", [Message]),
                    Pid ! {self(), Message}
            end;
        {Pid, {login, #{username := Username, password := Password}}} ->
            io:format("This user logged: ~p\n", [Username]),
            case registry:check_user(#{username => Username, password => Password}, Connection) of
                {ok, Email} ->
                    NewPid =
                        spawn(
                            ?MODULE,
                            handle_user,
                            [#{user_pid => Pid, connection => database:database_connect()}]
                        ),
                    io:format("User's email: ~p\n", [Email]),
                    Pid ! {ok, {NewPid, Email}};
                {error, Message} ->
                    io:format("Unexpected Error: ~p\n", [Message]),
                    Pid ! {error, Message}
            end
    end,
    handle_master(Connection).

stop(_) ->
    ok.

main(_) ->
    start(none, none).
