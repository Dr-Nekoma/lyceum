%%%-------------------------------------------------------------------
%% @doc This is the Backend's entrypoint, an application that runs our
%%      migrations and starts the main supervisor trees.
%% @end
%%%-------------------------------------------------------------------

-module(server).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

-spec start(term(), term()) -> gen_server:start_ret().
start(_StartType, _StartArgs) ->
    logger:info("Starting Server Application...~n"),
    server_sup:start_link().

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application has stopped. It
%% is intended to be the opposite of Module:start/2 and should do
%% any necessary cleaning up. The return value is ignored.
%% @spec stop(State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec stop(term()) -> ok.
stop(_State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
