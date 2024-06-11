%%%-------------------------------------------------------------------
%% @doc server public API
%% @end
%%%-------------------------------------------------------------------

-module(server).

-behaviour(application).

-export([start/2, stop/1, main/1, handle_message/1]).

-include("user_registry.hrl").
-include("user_login.hrl").

%% TODO: We shall remove the cookie given that this is a public game, lmao
start(_, _) ->
    Connection = user_handler:database_connect(),
    Pid = spawn(?MODULE, handle_message, [Connection]),
    erlang:register(lyceum_server, Pid),
    {ok, Pid}.

handle_message(Connection) ->
    receive
	{Pid, #{action := registration, username := Username, email := Email, password := Password}} ->
	    io:format("This user now exists: ~p", [Username]),
	    user_handler:insert_user(#user_registry{username = Username, 
						    password = Password,
						    email = Email},
				    Connection),
	    Response = "I registered " ++ Username,
	    Pid ! {self(), Response};
	{Pid, #{action := login, username := Username, password := Password}} ->
	    io:format("This user logged: ~p", [Username]),
	    user_handler:check_user(#user_login{username = Username, 
						password = Password},
				    Connection),
	    Response = "I found " ++ Username,
	    Pid ! {self(), {Response, 123, 
			    {1, #{something => "this is a string", 
				  another => test2, 
				  wtf => {test3, [true]}}}}};
        {Pid, Value} ->
	    io:format("Yo, we received something ~p ", [Value]),
	    Pid ! {self(), "Yo bruh, I got you xD"}
    end,    
    handle_message(Connection).

stop(_) ->
    ok.

main(_) ->
    start(none,none).
