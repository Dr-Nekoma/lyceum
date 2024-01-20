-module(user_handler).

-behavior(cowboy_rest).

-export([init/2, allowed_methods/2, content_types_provided/2, insert_user/1, registry/2]).

-record(user_registry, {username = none, password = none, email = none}).

insert_user(#user_registry{username = Username, password = Password, email = Email}, Connection) ->
    
    {ok, 1} = epgsql:squery(Connection, "INSERT INTO \"user\" VALUES (" ++  Username++ ", " ++ Password ++ ", " ++ Email ++ ")"),
    ok = epgsql:close(Connection).

insert_user(Registry) ->
    {ok, Connection} =
	epgsql:connect(#{host => "localhost",
			 username => "admin",
			 password => "admin",
			 database => "mmo",
			 timeout => 4000
			}),
    insert_user(Registry, Connection).

init(Request, State) ->
    {cowboy_rest, Request, State}.

allowed_methods(Request, State) ->
    {[<<"POST">>], Request, State}.

content_types_provided(Request, State) ->
    {[{{<<"text">>, <<"xml">>, []}, registry}], Request, State}.

registry(Request, State) ->
    {ok, Body, _} = cowboy_req:read_urlencoded_body(Request),
    case utilities:retrieve_input(Body) of
	{ok, Input} ->
	    ok
    end,
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

