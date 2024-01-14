%%%-------------------------------------------------------------------
%% @doc server public API
%% @end
%%%-------------------------------------------------------------------

-module(server).

-behaviour(application).

-export([start/2, stop/1, start/0]).

start(_StartType, _StartArgs) ->
    server_supervisor:start_link().

stop(_State) ->
    ok.

start() ->
    spawn(fun () -> {ok, Sock} = gen_tcp:listen(12321, [{packet, line}]),
                    echo_loop(Sock)
          end).

%% internal functions

echo_loop(Sock) ->
    {ok, Conn} = gen_tcp:accept(Sock),
    io:format("Got connection: ~p~n", [Conn]),
    Handler = spawn(fun () -> handle(Conn) end),
    gen_tcp:controlling_process(Conn, Handler),
    echo_loop(Sock).

handle(Conn) ->
    receive
        {tcp, Conn, Data} ->
            gen_tcp:send(Conn, Data),
            handle(Conn);
        {tcp_closed, Conn} ->
            io:format("Connection closed: ~p~n", [Conn])
    end.

postgres_test() ->
    {ok, Connection} =
	epgsql:connect(#{host => "localhost",
			 username => "admin",
			 password => "admin",
			 database => "mmo",
			 timeout => 4000
			}),
    epgsql:squery(Connection, "SELECT 'Hello World! :)'").


%% internal functions
