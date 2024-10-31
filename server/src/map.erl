-module(map).

-export([get_map/2]).

transform_tile_or_object(List) ->
    F = (fun(Map) -> #{name => erlang:binary_to_atom(maps:get(kind, Map)),
		       x_position => maps:get(x_position, Map),
		       y_position => maps:get(y_position, Map)} end),
    lists:map(F, List).

get_map(MapName, Connection) ->
    Result = epgsql:equery(Connection, "SELECT kind, x_position, y_position FROM map.tile WHERE map_name = $1::TEXT", [MapName]),
    Fun = fun (FullColumns, Values) -> 
		  Map = #{ map_name => MapName,
			   tiles => transform_tile_or_object(util:columns_and_rows(FullColumns, Values)),
			   objects => transform_tile_or_object(util:columns_and_rows(FullColumns, Values)) 
			 },
		  {ok, Map}
	   end,
    util:process_postgres_result(Result, select, Fun).

