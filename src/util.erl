-module(util).

-export([columns_and_rows/2]).

column_names(Columns) ->    
    lists:map(fun ({_,Name,Type,_,_,_,_,_,_}) -> {Type, erlang:binary_to_atom(Name)} end, Columns).

columns_and_rows(FullColumns, Rows) ->
    Columns = column_names(FullColumns),
    F = (fun (Row) ->
		 lists:zipwith(fun ({Type, ColumnName}, RowValue) -> 
				       Value = case Type of
						   varchar -> erlang:binary_to_list(RowValue);
						   text -> erlang:binary_to_list(RowValue);
						   _ -> RowValue
					       end,
				       {ColumnName, Value}
			       end, Columns, erlang:tuple_to_list(Row))
	 end),
    Values = lists:map(F, Rows),
    lists:map(fun maps:from_list/1, Values).
