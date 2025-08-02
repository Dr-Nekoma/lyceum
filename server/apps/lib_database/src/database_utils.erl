-module(database_utils).

-export([columns_and_rows/1, transform_character_map/1]).

-spec column_names(list()) -> list().
column_names(Columns) ->
    lists:map(fun({_, Name, Type, _, _, _, _, _, _}) -> {Type, erlang:binary_to_atom(Name)}
              end,
              Columns).

-spec columns_and_rows({list(), list()}) -> list().
columns_and_rows({_, []}) ->
    [];
columns_and_rows({FullColumns, Rows}) ->
    Columns = column_names(FullColumns),
    F = fun(Row) ->
           lists:zipwith(fun({Type, ColumnName}, RowValue) ->
                            Value =
                                case Type of
                                    varchar -> erlang:binary_to_list(RowValue);
                                    text -> erlang:binary_to_list(RowValue);
                                    _ -> RowValue
                                end,
                            {ColumnName, Value}
                         end,
                         Columns,
                         erlang:tuple_to_list(Row))
        end,
    Values = lists:map(F, Rows),
    lists:map(fun maps:from_list/1, Values).

-spec transform_character_map(list()) -> list().
transform_character_map(List) ->
    F = fun(Map) ->
           Map#{state_type :=
                    erlang:list_to_atom(
                        maps:get(state_type, Map))}
        end,
    lists:map(F, List).
