%%%-------------------------------------------------------------------
%% @doc server public API
%% @end
%%%-------------------------------------------------------------------

-module(server).

-behaviour(application).

-export([start/2, stop/1, main/1, handle_message/1]).

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
    Pid = spawn(?MODULE, handle_message, [Connection]),
    erlang:register(lyceum_server, Pid),
    {ok, Pid}.

handle_message(Connection) ->
    receive
	{Pid, #{action := registration, username := Username, email := Email, password := Password}} ->
	    io:format("This user now exists: ~p", [Username]),
	    user:insert_user(#{username => Username, 
			  password => Password,
			  email => Email},
			Connection),
	    Response = "I registered " ++ Username,
	    Pid ! {self(), Response};
	{Pid, #{action := login, username := Username, password := Password}} ->
	    io:format("This user logged: ~p", [Username]),
	    Email = user:check_user(#{username => Username, 
				      password => Password},
				    Connection),
	    io:format("User's email: ~p", [Email]),
	    Pid ! {self(), {ok, Email}};
	{Pid, #{action := character_list, username := Username, email := Email}} ->
	    io:format("Querying user's characters..."),
	    Characters = character:player_characters(Username, Email, Connection),
	    Pid ! {self(), {ok, Characters}};
	{Pid, #{action := character_creation} = Character_Map} ->
	    io:format("This character logged"),
	    character:create(Character_Map, Connection),
	    Pid ! {self(), "I created a character"};
        {Pid, Value} ->
	    io:format("Yo, we received something ~p ", [Value]),
	    Pid ! {self(), "Yo bruh, I got you xD"}
    end,    
    handle_message(Connection).

stop(_) ->
    ok.

main(_) ->
    start(none,none).
