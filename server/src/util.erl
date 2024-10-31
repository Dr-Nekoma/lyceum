-module(util).

-export([columns_and_rows/2, transform_character_map/1, process_postgres_result/3, psql_bind/2]).

column_names(Columns) ->
    lists:map(
        fun({_, Name, Type, _, _, _, _, _, _}) -> {Type, erlang:binary_to_atom(Name)} end, Columns
    ).

columns_and_rows(FullColumns, MaybeRows) ->
    case MaybeRows of
        [] ->
            [];
        Rows ->
            io:format("FullColumns: ~p, Rows: ~p\n", [FullColumns, Rows]),
            Columns = column_names(FullColumns),
            F =
                (fun(Row) ->
                    lists:zipwith(
                        fun({Type, ColumnName}, RowValue) ->
                            Value =
                                case Type of
                                    varchar -> erlang:binary_to_list(RowValue);
                                    text -> erlang:binary_to_list(RowValue);
                                    _ -> RowValue
                                end,
                            {ColumnName, Value}
                        end,
                        Columns,
                        erlang:tuple_to_list(Row)
                    )
                end),
            Values = lists:map(F, Rows),
            lists:map(fun maps:from_list/1, Values)
    end.

transform_character_map(List) ->
    F = (fun(Map) -> Map#{state_type := erlang:binary_to_atom(maps:get(state_type, Map))} end),
    lists:map(F, List).

process_postgres_result({ok, FullColumns, Values}, select, Fun) ->
    Fun(FullColumns, Values);
process_postgres_result({ok, Count}, update, Fun) ->
    Fun(Count);
process_postgres_result({ok, Count}, delete, Fun) ->
    Fun(Count);
process_postgres_result({ok, Count}, insert, Fun) ->
    Fun(Count);
process_postgres_result({ok, _, _, _}, insert, _) ->
    {error, "Unexpected use of Insert on Server side"};
process_postgres_result({error, Error}, Tag, _) ->
    io:format("Tag: ~p\nError: ~p\n", [Error, Tag]),
    {error, "Unexpected error (operation or PSQL) on Server side"}.

psql_bind(MonadicValue, []) -> MonadicValue;
psql_bind(ok, _) -> ok;
psql_bind({ok, Result}, [Fun | Tail]) -> psql_bind(Fun(Result), Tail);
psql_bind({error, _} = Error, _) -> Error;
psql_bind(_, _) -> {error, "Wrong monadic value in the chain"}.
