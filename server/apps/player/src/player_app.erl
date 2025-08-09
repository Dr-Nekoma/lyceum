%%%-------------------------------------------------------------------
%% @doc player public API
%% @end
%%%-------------------------------------------------------------------

-module(player_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    player_tl_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
