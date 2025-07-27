%%%-------------------------------------------------------------------
%% @doc world public API
%% @end
%%%-------------------------------------------------------------------

-module(world_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    logger:info("Starting ~p Application...~n", [?MODULE]),
    world_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
