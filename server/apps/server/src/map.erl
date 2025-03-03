-module(map).

-export([get_map/2]).
-compile({parse_transform, do}).

-spec atomize(binary()) -> atom().
atomize(Binary) -> list_to_atom(string:lowercase(binary_to_list(Binary))).

-spec transform_tile(map()) -> atom().
transform_tile(Map) -> atomize(maps:get(kind, Map)).

-spec transform_object(map()) -> atom().
transform_object(Map) -> atomize(maps:get(kind, Map)).

-spec transform_resource(map()) -> tuple().
transform_resource(Map) ->
    Position = {maps:get(x_position, Map), maps:get(y_position, Map)},
    NewMap = maps:update_with(kind, fun atomize/1, Map),
    {Position, maps:without([x_position, y_position], NewMap)}.

-spec check_dimensions(map()) -> any().
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

-spec get_map(epgsql:bind_param(), epgsql:connection()) -> any().
get_map(MapName, Connection) ->
    Dimensions =
        epgsql:equery(Connection,
                      "SELECT width, height FROM map.instance WHERE name = $1::TEXT",
                      [MapName]),
    Tiles =
        epgsql:equery(Connection,
                      "SELECT kind FROM map.tile WHERE map_name = $1::TEXT ORDER BY "
                      "y_position , x_position",
                      [MapName]),
    Objects =
        epgsql:equery(Connection,
                      "SELECT kind FROM map.object WHERE map_name = $1::TEXT ORDER "
                      "BY y_position, x_position",
                      [MapName]),
    Resources =
        epgsql:equery(Connection,
                      "SELECT x_position, y_position, quantity, kind, capacity, base_extraction_amount, base_extraction_time, item_pk FROM map.resource_view WHERE map_name = $1::TEXT",
                      [MapName]),
    do([postgres_m || 
	   UnprocessedMap <- {Dimensions, select},
	   {ok, {Width, Height}} = check_dimensions(UnprocessedMap), %% I miss you ErrorT
	   UnprocessedTiles <- {Tiles, select},
	   ProcessedTiles = lists:map(fun transform_tile/1, database_utils:columns_and_rows(UnprocessedTiles)),
	   UnprocessedObjects <- {Objects, select},
	   ProcessedObjects = lists:map(fun transform_object/1, database_utils:columns_and_rows(UnprocessedObjects)),
	   UnprocessedResources <- {Resources, select},
	   ProcessedResources = lists:map(fun transform_resource/1, database_utils:columns_and_rows(UnprocessedResources)),
	   Quantity = Width * Height,
	   if (length(ProcessedTiles) == Quantity) and (length(ProcessedObjects) == Quantity) ->
		   return(#{tiles => ProcessedTiles,
			    objects => ProcessedObjects,
			    resources => ProcessedResources,
			    width => Width,
			    height => Height});
	      (length(ProcessedTiles) == 0) or (length(ProcessedObjects) == 0) ->
		   % TODO: Put the map name in this error message for the client!
		   fail("Map can't be instantiated!");
	      true -> fail("Mismatch between dimensions, tiles and objects!")
	   end]).
