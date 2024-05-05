%%%-------------------------------------------------------------------
%% @doc server public API
%% @end
%%%-------------------------------------------------------------------

-module(server).

%-behaviour(application).

-export([start/1, init/1, start/2, start/0, stop/1, stop/0]).
-export([foo/1, bar/1, lyceum_wrapper/0, receive_whatever/0]).

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([{'_', [{"/", hello_handler, []},
					     {"/user", user_handler, []}]}]),
    {ok, _} =
        cowboy:start_clear(my_http_listener, [{port, 7070}], #{env => #{dispatch => Dispatch}}),
    server_supervisor:start_link().

lyceum_wrapper() ->
    erlang:set_cookie(lyceum),
    Pid = spawn(?MODULE, receive_whatever, []),
    erlang:register(lyceum_server, Pid).

receive_whatever() ->
    receive
	{Pid, Value} ->
	    io:format("Yo, we received something ~p ", [Value]),
	    Pid ! {self(), "Yo bruh, I got you xD"}	    
    end,    
    receive_whatever().

start() ->
    start(none, none).

stop(_State) ->
    ok.

start(ExtPrg) ->
    spawn(?MODULE, init, [ExtPrg]).
stop() ->
    complex ! stop.

foo(X) ->
    call_port({foo, X}).
bar(Y) ->
    call_port({bar, Y}).

call_port(Msg) ->
    complex ! {call, self(), Msg},
    receive
	{complex, Result} ->
	    Result
    end.

init(ExtPrg) ->
    register(complex, self()),
    process_flag(trap_exit, true),
    Port = open_port({spawn, ExtPrg}, [{packet, 2}, binary]),
    loop(Port).

loop(Port) ->
    receive
	{call, Caller, Msg} ->
	    Port ! {self(), {command, term_to_binary(Msg)}},
	    receive
		{Port, {data, Data}} ->
		    Caller ! {complex, binary_to_term(Data)}
	    end,
	    loop(Port);
	stop ->
	    Port ! {self(), close},
	    receive
		{Port, closed} ->
		    exit(normal)
	    end;
	{'EXIT', Port, Reason} ->
	    exit(port_terminated)
    end.

encode({foo, X}) -> [1, X];
encode({bar, Y}) -> [2, Y].

decode([Int]) -> Int.
