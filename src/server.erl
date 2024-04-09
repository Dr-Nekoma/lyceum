%%%-------------------------------------------------------------------
%% @doc server public API
%% @end
%%%-------------------------------------------------------------------

-module(server).

-behaviour(application).

-export([start/2, start/0, stop/1]).

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([{'_', [{"/", hello_handler, []},
					     {"/user", user_handler, []}]}]),
    {ok, _} =
        cowboy:start_clear(my_http_listener, [{port, 7070}], #{env => #{dispatch => Dispatch}}),
    server_supervisor:start_link().

start() ->
    start(none, none).

stop(_State) ->
    ok.
