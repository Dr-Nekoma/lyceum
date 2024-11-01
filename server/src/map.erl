-module(map).

-export([get_map/2]).

transform_tile_or_object(List) ->
    F = (fun(Map) -> #{name => list_to_atom(string:lowercase(binary_to_list((maps:get(kind, Map))))),
		       x_position => maps:get(x_position, Map),
		       y_position => maps:get(y_position, Map)} end),
    lists:map(F, List).

get_map(MapName, Connection) ->
    Dimensions = epgsql:equery(Connection, "SELECT width, height FROM map.instance WHERE name = $1::TEXT", [MapName]),
    Tiles = epgsql:equery(Connection, "SELECT kind, x_position, y_position FROM map.tile WHERE map_name = $1::TEXT", [MapName]),
    Objects = epgsql:equery(Connection, "SELECT kind, x_position, y_position FROM map.object WHERE map_name = $1::TEXT", [MapName]),
    FunDimensions = fun (FullColumns, Values) -> {ok, util:columns_and_rows(FullColumns, Values)} end,
    FunTilesObjects = fun (FullColumns, Values) -> {ok, transform_tile_or_object(util:columns_and_rows(FullColumns, Values))} end,
    util:psql_bind(util:process_postgres_result(Dimensions, select, FunDimensions),
		   [fun (ListDimensionsMap) ->
			    case ListDimensionsMap of
				[Map] -> Width = maps:get(width, Map),
					 Height = maps:get(height, Map),
					 {ok, {Width, Height}};
				_ -> io:format("[ERROR] Something to wrong when getting map dimensions!\n"), exit(1)
			    end
		    end,
		    fun ({Width, Height}) ->
			    util:psql_bind(util:process_postgres_result(Tiles, select, FunTilesObjects),
					   [fun (TilesV) -> 
						    util:psql_bind(util:process_postgres_result(Objects, select, FunTilesObjects),
								   [fun (ObjectsV) -> 
									    Quantity = Width * Height,
									    if 
										(length(TilesV) == Quantity) and (length(ObjectsV) == Quantity) ->
										    {ok, #{tiles => TilesV,
											   objects => ObjectsV,
											   width => Width,
											   height => Height}};
										true -> {error, "Mismatch between dimensions, tiles and objects!"}
									    end
								    end]) 
					    end])
		    end]).
