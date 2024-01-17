-module(hello_handler).

-behavior(cowboy_rest).

-export([init/2, allowed_methods/2, content_types_provided/2, hello/2]).

init(Request, State) ->
    {cowboy_rest, Request, State}.

allowed_methods(Request, State) ->
    {[<<"GET">>], Request, State}.

content_types_provided(Request, State) ->
    {[{{<<"text">>, <<"xml">>, []}, hello}], Request, State}.

hello(Request, State) ->
    Message = [{response, [{version, "0.1"}], [{message, [], [postgres_hello()]}]}],
    {utilities:encode(Message), Request, State}.

postgres_hello() ->
    {ok, Connection} =
        epgsql:connect(#{host => "localhost",
                         username => "admin",
                         password => "admin",
                         database => "mmo",
                         timeout => 4000}),
    {ok, _Header, [{Message}]} = epgsql:squery(Connection, "SELECT 'Hello! :)'"),
    binary_to_list(Message).

