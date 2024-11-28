-module(map).

-export([get_map/2]).
-compile({parse_transform, do}).

transform_tile(Map) ->
    list_to_atom(string:lowercase(binary_to_list(maps:get(kind, Map)))).

transform_object(Map) ->
    list_to_atom(string:lowercase(binary_to_list(maps:get(kind, Map)))).

check_dimensions(UnprocessedMap) ->
    do([error_m ||	   
	   case database_utils:columns_and_rows(UnprocessedMap) of
	       [Map] ->
		   Width = maps:get(width, Map),
		   Height = maps:get(height, Map),
		   return({Width, Height});
	       _ -> io:format("[ERROR] Something to wrong when getting map dimensions!\n"),
		    exit(2)
	   end]).

get_map(MapName, Connection) ->
    Dimensions =
        epgsql:equery(Connection,
                      "SELECT width, height FROM map.instance WHERE name = $1::TEXT",
                      [MapName]),
    Tiles =
        epgsql:equery(Connection,
                      "SELECT kind FROM map.tile WHERE map_name = $1::TEXT ORDER BY "
                      "x_position , y_position",
                      [MapName]),
    Objects =
        epgsql:equery(Connection,
                      "SELECT kind FROM map.object WHERE map_name = $1::TEXT ORDER "
                      "BY x_position, y_position",
                      [MapName]),
    do([postgres_m || 
	   UnprocessedMap <- {Dimensions, select},
	   {ok, {Width, Height}} = check_dimensions(UnprocessedMap), %% I miss you ErrorT
	   UnprocessedTiles <- {Tiles, select},
	   ProcessedTiles = lists:map(fun transform_tile/1, database_utils:columns_and_rows(UnprocessedTiles)),
	   UnprocessedObjects <- {Objects, select},
	   ProcessedObjects = lists:map(fun transform_object/1, database_utils:columns_and_rows(UnprocessedObjects)),
	   Quantity = Width * Height,
	   if (length(ProcessedTiles) == Quantity) and (length(ProcessedObjects) == Quantity) ->
		   return(#{tiles => ProcessedTiles,
			    objects => ProcessedObjects,
			    width => Width,
			    height => Height});
	      (length(ProcessedTiles) == 0) or (length(ProcessedObjects) == 0) ->
		   % TODO: Put the map name in this error message for the client!
		   fail("Map can't be instantiated!");
	      true -> fail("Mismatch between dimensions, tiles and objects!")
	   end]).
