%%%-------------------------------------------------------------------
%% @doc crud public API
%% @end
%%%-------------------------------------------------------------------

-module(crud_app).

-behaviour(application).

-export([start/2, stop/1, postgres_test/0, calculate_area/0, area/0]).

start(_StartType, _StartArgs) ->
    crud_sup:start_link().

stop(_State) ->
    ok.

postgres_test() ->
    {ok, Connection} =
	epgsql:connect(#{host => "localhost",
			 username => "admin",
			 password => "admin",
			 database => "crud",
			 timeout => 4000
			}),
    epgsql:squery(Connection, "SELECT 'Hello World! :)'").

%% -spec area({atom(), term()}) -> integer().

%% area({square, X}) ->
%%     X*X;

%% area({rectangle, X, Y}) ->
%%     X*Y.

-spec area() -> integer().

area() ->
    receive
	{From, square, X} -> 
	    From ! {self(), X*X};
	{From, rectangle, X, Y} -> From ! {self(), X*Y}
    end,
    area().

calculate_area() ->
    Pid = spawn(crud_app, area, []),
    Pid ! {self(), square, 2},
    receive
	{_, Reply} -> io:format(integer_to_list(Reply))
    end.

%% remote_call(Pid, Request) ->
%%     Tag = erlang:make_ref(),
%%     Pid ! {self(), Tag, Request},
%%     receive
%% 	{Tag, Response} ->
%% 	    Response
%%     end.

%% internal functions
