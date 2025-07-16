%%%-------------------------------------------------------------------
%% @doc storage_pgsql public API
%% @end
%%%-------------------------------------------------------------------

-module(storage_pgsql_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    storage_pgsql_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
