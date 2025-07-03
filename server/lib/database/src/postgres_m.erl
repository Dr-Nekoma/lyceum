-module(postgres_m).

%% TODO: Create our own behaviour that has better epgsql types
%% to feed it into return, fail and >>=.
-behaviour(monad).

-export(['>>='/2, return/1, fail/1]).

%% TODO: fix these types
-dialyzer({nowarn_function, ['>>='/2, return/1, fail/1]}).

-type monad(_A) ::
    {{ok, any(), any()}, select} |
    {{ok, any(), any()}, call} |
    {{ok, any()}, delete} |
    {{ok, any()}, insert} |
    {{ok, any(), any(), any()}, insert} |
    {{ok, any()}, update} |
    {{error, any()}, any()}.

-include_lib("erlandono/include/monad_specs.hrl").

return(X) ->
    {ok, X}.

fail(X) ->
    {error, X}.

'>>='({{ok, FullColumns, Values}, select}, Fun) ->
    Fun({FullColumns, Values});
'>>='({{ok, FullColumns, Values}, call}, Fun) ->
    Fun({FullColumns, Values});
'>>='({{ok, Count}, delete}, Fun) ->
    Fun(Count);
'>>='({{ok, Count}, insert}, Fun) ->
    Fun(Count);
'>>='({{ok, _, _, _}, insert}, _) ->
    fail("Unexpected use of Insert on Server side");
'>>='({{ok, Count}, update}, Fun) ->
    Fun(Count);
'>>='({{error, Error}, Tag}, _) ->
    logger:error("Tag: ~p\nError: ~p\n", [Error, Tag]),
    fail("Unexpected error (operation or PSQL) on Server side");
'>>='(X, Y) ->
    logger:error("Tag: ~p\nError: ~p\n", [X, Y]),
    fail("Unexpected error (operation or PSQL) on Server side").
