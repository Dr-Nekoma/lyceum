%%%-------------------------------------------------------------------
%% @doc server public API
%% @end
%%%-------------------------------------------------------------------

-module(server).

-behaviour(application).

-export([start/2, stop/1, main/1, handle_master/1, handle_user/1, stream_user_data/1]).

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

stream_user_data(#{user_pid := UserPid, connection := Connection} = State) ->
    {ok, Data} = epgsql:set_notice_receiver(Connection, self()),    
    io:format("Data: ~p\n", [Data]),
    %% UserPid ! {ok, Data},
    stream_user_data(State).

%% timeout_user(State) ->

% TODO: We should not just keep passing Connections here, this is dangerous for interactive stuff
handle_user(#{user_pid := UserPid, connection := Connection} = State) ->
    receive	    
	{list_characters, #{username := Username, email := Email}} ->
	    io:format("Querying user's characters..."),
	    Characters = character:player_characters(Username, Email, Connection),
	    UserPid ! {ok, Characters};
	{create_character, Character_Map} ->
	    io:format("This character logged"),
	    character:create(Character_Map, Connection),
	    UserPid ! ok;
	joining_world -> 
	    case maps:is_key(State, stream_pid) of
		true ->
		    io:format("We tried to join two worlds at once, bruh. Stopping right now\n"),
		    erlang:exit(double_joining);
		false -> NewState = State#{handler_pid := self()}, 
			 StreamPid = spawn(?MODULE, stream_user_data, [NewState]),
			 NewState = State#{stream_pid := StreamPid},
			 %% TimerOutPid = spawn(?MODULE, timeout_user, [NewState]),
			 handle_user(NewState)
	    end;
	{update_character, Character_Map} ->
	    io:format("Character will be updated"),
	    character:updateTemp(Character_Map, Connection),
	    UserPid ! ok;
	{retrieve_character, Character_Map} ->
	    io:format("Character will be retrieved"),
	    Data = character:retrieve(Character_Map, Connection),
	    UserPid ! Data;
	Something ->
	    io:format("Catch all case: ~p", [Something])
    end,
    handle_user(State).

handle_master(Connection) ->
    receive
	{Pid, {register, #{username := Username, email := Email, password := Password}}} ->
	    io:format("This user now exists: ~p", [Username]),
	    user:insert_user(#{username => Username, 
			  password => Password,
			  email => Email},
			Connection),
	    Response = "I registered " ++ Username,
	    Pid ! {self(), Response};
	{Pid, {login, #{username := Username, password := Password}}} ->
	    io:format("This user logged: ~p", [Username]),
	    Email = user:check_user(#{username => Username, 
				      password => Password},
				    Connection),
	    NewPid = spawn(?MODULE, handle_user, [#{ user_pid => Pid, connection => Connection}]),
	    io:format("User's email: ~p", [Email]),
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
