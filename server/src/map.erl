-module(map).

-export([get_map/2]).

transform_tile_or_object(List) ->
    F = (fun(Map) -> #{name => list_to_atom(string:lowercase(binary_to_list((maps:get(kind, Map))))),
		       x_position => maps:get(x_position, Map),
		       y_position => maps:get(y_position, Map)} end),
    lists:map(F, List).

get_map(MapName, Connection) ->
    Tiles = epgsql:equery(Connection, "SELECT kind, x_position, y_position FROM map.tile WHERE map_name = $1::TEXT", [MapName]),
    Fun = fun (FullColumns, Values) -> {ok, transform_tile_or_object(util:columns_and_rows(FullColumns, Values))} end,
    Objects = epgsql:equery(Connection, "SELECT kind, x_position, y_position FROM map.object WHERE map_name = $1::TEXT", [MapName]),
    util:psql_bind(util:process_postgres_result(Tiles, select, Fun),
		   [(fun (TilesV) -> util:psql_bind(util:process_postgres_result(Objects, select, Fun),
						    [(fun (ObjectsV) -> {ok, #{tiles => TilesV,
									       objects => ObjectsV}} end)]) end)]).

