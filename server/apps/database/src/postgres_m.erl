-module(postgres_m).

%% Monad for chaining pgo query results.
%%
%% pgo:query/3 returns either:
%%   #{command := select|insert|update|delete|call|..., num_rows := N, rows := Rows}
%%   {error, Reason}
%%
%% This monad threads success values through `>>=' and short-circuits
%% on errors. For SELECT/CALL the Fun receives the row list; for
%% mutations it receives the row count.

-behaviour(monad).

-export(['>>='/2, return/1, fail/1]).

-dialyzer({nowarn_function, ['>>='/2, return/1, fail/1]}).

-type monad(_A) :: database:query_result() | {ok, term()} | {error, term()}.

-include_lib("erlandono/include/monad_specs.hrl").

return(X) ->
    {ok, X}.

fail(X) ->
    {error, X}.

'>>='(#{command := select, rows := Rows}, Fun) ->
    Fun(Rows);
'>>='(#{command := call, rows := Rows}, Fun) ->
    Fun(Rows);
'>>='(#{command := insert, num_rows := N}, Fun) ->
    Fun(N);
'>>='(#{command := update, num_rows := N}, Fun) ->
    Fun(N);
'>>='(#{command := delete, num_rows := N}, Fun) ->
    Fun(N);
'>>='(#{command := _Other, rows := Rows}, Fun) ->
    %% e.g. `set' from `SET TRANSACTION ISOLATION LEVEL ...'
    Fun(Rows);
'>>='({ok, Value}, Fun) ->
    Fun(Value);
'>>='({error, Reason}, _) ->
    logger:error("[~p] PSQL error: ~p~n", [?MODULE, Reason]),
    fail("Unexpected error (operation or PSQL) on Server side");
'>>='(Unexpected, _) ->
    logger:error("[~p] Unexpected monad value: ~p~n", [?MODULE, Unexpected]),
    fail("Unexpected error (operation or PSQL) on Server side").
