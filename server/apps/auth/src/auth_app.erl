%%%-------------------------------------------------------------------
%% @doc auth public API
%% @end
%%%-------------------------------------------------------------------

-module(auth_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    auth_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
