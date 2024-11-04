-module(map).

-export([get_map/2]).

transform_tile(Map) -> list_to_atom(string:lowercase(binary_to_list((maps:get(kind, Map))))).
transform_object(Map) -> list_to_atom(string:lowercase(binary_to_list((maps:get(kind, Map))))).
    
get_map(MapName, Connection) ->
    Dimensions = epgsql:equery(Connection, "SELECT width, height FROM map.instance WHERE name = $1::TEXT", [MapName]),
    Tiles = epgsql:equery(Connection, "SELECT kind FROM map.tile WHERE map_name = $1::TEXT ORDER BY x_position , y_position", [MapName]),
    Objects = epgsql:equery(Connection, "SELECT kind FROM map.object WHERE map_name = $1::TEXT ORDER BY x_position, y_position", [MapName]),
    FunDimensions = fun (FullColumns, Values) -> {ok, util:columns_and_rows(FullColumns, Values)} end,
    FunTiles = fun (FullColumns, Values) -> {ok, lists:map(fun transform_tile/1, (util:columns_and_rows(FullColumns, Values)))} end,
    FunObjects = fun (FullColumns, Values) -> {ok, lists:map(fun transform_object/1, (util:columns_and_rows(FullColumns, Values)))} end,
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
			    util:psql_bind(util:process_postgres_result(Tiles, select, FunTiles),
					   [fun (TilesV) -> 
						    util:psql_bind(util:process_postgres_result(Objects, select, FunObjects),
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
