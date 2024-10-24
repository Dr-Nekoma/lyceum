%%%-------------------------------------------------------------------
%% @doc server top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(server_supervisor).

-behaviour(supervisor).

-export([start_link/0, init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    %% https://www.erlang.org/doc/system/sup_princ.html#supervisor-flags
    SupFlags = {one_for_one, 2, 10},
    %% https://www.erlang.org/doc/system/sup_princ.html#child-specification
    ChildSpec =
        {server,
         {server, start_link, []},
         %% The process will always be restarted
         permanent,
         %% Time (in ms) to wait between restarts
         2000,
         %% Type
         worker,
         [server]},

    {ok,
     {SupFlags,
      [ChildSpec]}}.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          %% The process will always be restarted

%% internal functions
