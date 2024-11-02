-module(map).

-export([get_map/2]).

transform_tile_or_object(List) ->
    F = (fun(Map) -> list_to_atom(string:lowercase(binary_to_list((maps:get(kind, Map))))) end),
    lists:map(F, List).
    
get_map(MapName, Connection) ->
    Dimensions = epgsql:equery(Connection, "SELECT width, height FROM map.instance WHERE name = $1::TEXT", [MapName]),
    Tiles = epgsql:equery(Connection, "SELECT kind FROM map.tile WHERE map_name = $1::TEXT ORDER BY x_position, y_position DESC", [MapName]),
    Objects = epgsql:equery(Connection, "SELECT kind FROM map.object WHERE map_name = $1::TEXT ORDER BY x_position, y_position DESC", [MapName]),
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
										(length(TilesV) == 0) or (length(ObjectsV) == 0) ->
										    % TODO: Put the map name in this error message for the client!
										    {error, "Map can't be instantiated!"};
										true -> {error, "Mismatch between dimensions, tiles and objects!"}
									    end
								    end]) 
					    end])
		    end]).
