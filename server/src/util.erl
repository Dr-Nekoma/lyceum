-module(util).

-export([columns_and_rows/2, transform_character_map/1, process_postgres_result/3]).

column_names(Columns) ->
    lists:map(
        fun({_, Name, Type, _, _, _, _, _, _}) -> {Type, erlang:binary_to_atom(Name)} end, Columns
    ).

columns_and_rows(FullColumns, MaybeRows) ->
    case MaybeRows of
	[] -> [];
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
process_postgres_result({ok, Count, Columns, Rows}, insert, Fun) ->
    Fun(Count, Columns, Rows);
process_postgres_result(Error, _, _) -> {error, Error}.
		    
