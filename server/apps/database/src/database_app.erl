%%%-------------------------------------------------------------------
%% @doc database public API. Owns the pgo pools that the rest of the
%%      umbrella reaches into for PostgreSQL access.
%% @end
%%%-------------------------------------------------------------------
-module(database_app).

-behaviour(application).

-export([start/2, stop/1]).

-spec start(application:start_type(), term()) -> {ok, pid()} | {error, term()}.
start(_StartType, _StartArgs) ->
    logger:info("[~p] Starting Application...~n", [?MODULE]),
    database_sup:start_link().

-spec stop(term()) -> ok.
stop(_State) ->
    ok.
