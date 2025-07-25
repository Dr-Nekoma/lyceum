%%%-------------------------------------------------------------------
%% @doc storage_mnesia public API
%% @end
%%%-------------------------------------------------------------------

-module(storage_mnesia_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    logger:info("[~p] Starting Application...~n", [?MODULE]),
    storage_mnesia_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
