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

process_postgres_result(Result, Tag, Fun) ->
    case Tag of
	select -> case Result of
		      {ok, FullColumns, Values} -> Fun(FullColumns, Values);
		      _ -> {error, "Unexpected result from Select"}
		  end;
	update -> case Result of
		      {ok, Count} -> Fun(Count);
		      _ -> {error, "Unexpected result from Update"}
		  end;
	delete -> case Result of
		      {ok, Count} -> Fun(Count);
		      _ -> {error, "Unexpected result from Delete"}
		  end;
	insert -> case Result of
		      {ok, Count, Columns, Rows} -> Fun(Count, Columns, Rows);
		      _ -> {error, "Unexpected result from Insert"}
		  end;
	_ -> {error, "Unrecognized operation in Server side"}
    end.
			  
			   

		    
