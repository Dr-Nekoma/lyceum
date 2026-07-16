-module(map).

-export([get_map/2]).

-compile({parse_transform, do}).

-spec atomize(binary() | list()) -> atom().
atomize(Binary) when is_binary(Binary) ->
    list_to_atom(string:lowercase(binary_to_list(Binary)));
atomize(Str) when is_list(Str) ->
    list_to_atom(string:lowercase(Str)).

-spec transform_tile(map()) -> atom().
transform_tile(Map) ->
    atomize(maps:get(kind, Map)).

-spec transform_object(map()) -> atom().
transform_object(Map) ->
    atomize(maps:get(kind, Map)).

-spec transform_resource(map()) -> tuple().
transform_resource(Map) ->
    Position = {maps:get(x_position, Map), maps:get(y_position, Map)},
    NewMap = maps:update_with(kind, fun atomize/1, Map),
    {Position, maps:without([x_position, y_position], NewMap)}.

-spec check_dimensions([map()]) -> any().
check_dimensions(UnprocessedMap) ->
    do([
        error_m
     || case database_utils:columns_and_rows(UnprocessedMap) of
            [Map] ->
                Width = maps:get(width, Map),
                Height = maps:get(height, Map),
                return({Width, Height});
            _ ->
                logger:error(
                    "[~p] Something to wrong when getting map dimensions!~n",
                    [?MODULE]
                ),
                exit(2)
        end
    ]).

-spec get_map(iodata(), database:pool_name()) -> any().
get_map(MapName, Pool) ->
    DimensionsQuery = database_queries:fetch_query("map", "select_map_dimensions.sql"),
    Dimensions = database:query(Pool, DimensionsQuery, [MapName]),
    TilesQuery = database_queries:fetch_query("map", "select_tiles.sql"),
    Tiles = database:query(Pool, TilesQuery, [MapName]),
    ObjectsQuery = database_queries:fetch_query("map", "select_map_objects.sql"),
    Objects = database:query(Pool, ObjectsQuery, [MapName]),
    ResourcesQuery = database_queries:fetch_query("map", "select_map_resources.sql"),
    Resources = database:query(Pool, ResourcesQuery, [MapName]),
    do([
        postgres_m
     || UnprocessedMap <- Dimensions,
        %% I miss you ErrorT
        {ok, {Width, Height}} = check_dimensions(UnprocessedMap),
        UnprocessedTiles <- Tiles,
        ProcessedTiles =
            lists:map(fun transform_tile/1, database_utils:columns_and_rows(UnprocessedTiles)),
        UnprocessedObjects <- Objects,
        ProcessedObjects =
            lists:map(
                fun transform_object/1,
                database_utils:columns_and_rows(UnprocessedObjects)
            ),
        UnprocessedResources <- Resources,
        ProcessedResources =
            lists:map(
                fun transform_resource/1,
                database_utils:columns_and_rows(UnprocessedResources)
            ),
        Quantity = Width * Height,
        if
            (length(ProcessedTiles) == Quantity) and (length(ProcessedObjects) == Quantity) ->
                return(#{
                    tiles => ProcessedTiles,
                    objects => ProcessedObjects,
                    resources => ProcessedResources,
                    width => Width,
                    height => Height
                });
            (length(ProcessedTiles) == 0) or (length(ProcessedObjects) == 0) ->
                fail("Map can't be instantiated!");
            true ->
                fail("Mismatch between dimensions, tiles and objects!")
        end
    ]).
