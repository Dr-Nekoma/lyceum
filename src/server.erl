%%%-------------------------------------------------------------------
%% @doc server public API
%% @end
%%%-------------------------------------------------------------------

-module(server).

-behaviour(application).

-export([start/2, stop/1, main/1, handle_master/1, handle_user/1]).

-define(PGHOST, os:getenv("PGHOST", "127.0.0.1")).
-define(PGPORT, os:getenv("PGPORT", 5432)).
-define(PGUSER, os:getenv("PGUSER", "admin")).
-define(PGPASSWORD, os:getenv("PGPASSWORD", "admin")).
-define(PGDATABASE, os:getenv("PGDATABASE", "mmo")).


database_connect() ->
    {ok, Connection} =
        epgsql:connect(
            #{ host => ?PGHOST,
               username => ?PGUSER,
               password => ?PGPASSWORD,
               database => ?PGDATABASE,
               timeout => 4000 }),
    Connection.

    %% ok = epgsql:close(Connection).
%% TODO: We shall remove the cookie given that this is a public game, lmao
start(_, _) ->
    Connection = database_connect(),
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
	    Characters = character:player_characters(Username, Email, Connection),
	    UserPid ! {ok, Characters};
	{create_character, Character_Map} ->
	    io:format("This character logged"),
	    character:create(Character_Map, Connection),
	    UserPid ! ok;
	{joining_map, Character_Map} -> 
	    io:format("User is about to join a world!\n"),
	    case character:activate(Character_Map, erlang:pid_to_list(UserPid), Connection) of
		ok -> io:format("Everything went ok with User: ~p!\n", [UserPid]),
		      UserPid ! ok;
		already_active -> io:format("Double joining\n"),
				  exit(1);
		{error, Message} -> io:format("Unexpected Error: ~p\n", [Message]),
				    exit(2)
	    end;
	exit_map ->
	    case character:deactivate(erlang:pid_to_list(UserPid), Connection) of
		ok -> UserPid ! ok;
		already_inactive -> io:format("Double leaving\n"),
				    exit(1);
		{error, Message} -> io:format("Unexpected Error: ~p\n", [Message]),
				    exit(2)
		      
	    end;
	disconnect ->
	    character:deactivate(erlang:pid_to_list(UserPid), Connection),
	    epgsql:close(Connection),
	    UserPid ! ok,
	    exit(0);	    
	{update_character, Character_Map} ->
	    io:format("Character will be updated. User: ~p\n", [UserPid]),
	    character:updateTemp(Character_Map, Connection),
	    io:format("Retrieving nearby characters...\n"),
	    Players = character:retrieve_near_players(Character_Map, erlang:pid_to_list(UserPid), Connection),
	    io:format("Everything went ok!\n"),
	    UserPid ! {ok, Players}
    end,
    handle_user(State).

handle_master(Connection) ->
    receive
	{Pid, {register, #{username := Username, email := Email, password := Password}}} ->
	    io:format("This user now exists: ~p", [Username]),
	    registry:insert_user(#{username => Username, 
			  password => Password,
			  email => Email},
			Connection),
	    Response = "I registered " ++ Username,
	    Pid ! {self(), Response};
	{Pid, {login, #{username := Username, password := Password}}} ->
	    io:format("This user logged: ~p\n", [Username]),
	    Email = registry:check_user(#{username => Username, 
				      password => Password},
				    Connection),
	    NewPid = spawn(?MODULE, handle_user, [#{ user_pid => Pid, connection => database_connect()}]),
	    io:format("User's email: ~p\n", [Email]),
	    Pid ! {ok, {NewPid, Email}};
        {Pid, Value} ->
	    io:format("Yo, we received something ~p ", [Value]),
	    Pid ! {self(), "Yo bruh, I got you xD"}
    end,    
    handle_master(Connection).

stop(_) ->
    ok.

main(_) ->
    start(none,none).
