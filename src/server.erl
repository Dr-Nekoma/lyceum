%%%-------------------------------------------------------------------
%% @doc server public API
%% @end
%%%-------------------------------------------------------------------

-module(server).

-behaviour(application).

-export([start/2, stop/1, main/1, handle_message/1]).

database_connect() ->
    {ok, Connection} =
	epgsql:connect(#{host => "localhost",
			 username => "admin",
			 password => "admin",
			 database => "mmo",
			 timeout => 4000
			}),
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
	    user:check_user(#{username => Username, 
			       password => Password},
			     Connection),
	    Response = "I found " ++ Username,
	    Pid ! {self(), Response};
	{Pid, #{action := character_creation, username := Username, email := Email, password := Password}} ->
	    io:format("This character logged: ~p", [Username]),
	    character:create(#{username => Username, 
			       password => Password,
			       email => Email},
			     Connection),
	    Response = "I found " ++ Username,
	    Pid ! {self(), ok};
        {Pid, Value} ->
	    io:format("Yo, we received something ~p ", [Value]),
	    Pid ! {self(), "Yo bruh, I got you xD"}
    end,    
    handle_message(Connection).

stop(_) ->
    ok.

main(_) ->
    start(none,none).
