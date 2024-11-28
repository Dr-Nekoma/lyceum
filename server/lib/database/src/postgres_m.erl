-module(postgres_m).

-behaviour(monad).
-export(['>>='/2, return/1, fail/1]).

-type(monad(_A) :: {{'ok', any(), any()}, 'select'} 
		 | {{'ok', any()}, 'delete'}
		 | {{'ok', any()}, 'insert'}
		 | {{'ok', any(), any(), any()}, 'insert'}
		 | {{'ok', any()}, 'update'}
		 | {{'error', any()}, any()}).
-include_lib("erlando/include/monad_specs.hrl").

'>>='({{ok, FullColumns, Values}, select}, Fun) -> Fun(FullColumns, Values);
'>>='({{ok, Count}, delete}, Fun) -> Fun(Count);
'>>='({{ok, Count}, insert}, Fun) -> Fun(Count);
'>>='({{ok, _, _, _}, insert}, _) -> {error, "Unexpected use of Insert on Server side"};
'>>='({{ok, Count}, update}, Fun) -> Fun(Count);
'>>='({{error, Error}, Tag}, _) -> 
    io:format("Tag: ~p\nError: ~p\n", [Error, Tag]),
    {error, "Unexpected error (operation or PSQL) on Server side"}.

return(X) -> X.
fail(X) -> {error, X}.

