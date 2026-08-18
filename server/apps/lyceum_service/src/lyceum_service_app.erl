-module(lyceum_service_app).
-moduledoc """
Application callback for `lyceum_service`.
""".

-behaviour(application).

-export([start/2, stop/1]).

-spec start(application:start_type(), term()) -> {ok, pid()} | {error, term()}.
start(_StartType, _StartArgs) ->
    lyceum_service_sup:start_link().

-spec stop(term()) -> ok.
stop(_State) ->
    ok.
