-module(map_generator).

-export([create_map/3]).
-compile({parse_transform, do}).

create_map(Connection, MapPath, MapName) ->
    create_tiles(Connection, MapPath, MapName),
    create_objects(Connection, MapPath, MapName).

create_tiles(Connection, MapPath, MapName) ->
    TilePath = filename:join([MapPath, MapName, "tile.csv"]),
    Tiles = fetch_file(TilePath),
    generate(Connection, MapName, Tiles).

create_objects(Connection, MapPath, MapName) ->
    ObjectsPath = filename:join([MapPath, MapName, "object.csv"]),
    Objects = fetch_file(ObjectsPath),
    generate(Connection, MapName, Objects).

fetch_file(CsvPath) ->
    {ok, Bin} = file:read_file(CsvPath),
    Content = csv:decode_binary(Bin),
    {case filename:basename(CsvPath) of
         "tile.csv" ->
             "tile";
         "object.csv" ->
             "object"
     end,
     Content}.

-spec generate(epgsql:connection(), epgsql:bind_param(), {epgsql:bind_param(), list()}) -> any().
generate(Connection, MapName, {Name, Table}) ->
    MapAttributes =
        lists:join(",",
                   lists:map(fun ({I, J, ""}) ->
                                     io_lib:format("('~s', '~s', ~B, ~B)",
                                                   [MapName, "EMPTY", I, J]);
                                 ({I, J, Elem}) ->
                                     io_lib:format("('~s', '~s', ~B, ~B)",
                                                   [MapName, string:uppercase(Elem), I, J])
                             end,
                             lists:flatten(
                                 lists:map(fun({I, Out}) ->
                                              lists:map(fun({J, In}) -> {I, J, In} end,
                                                        lists:enumerate(0, Out))
                                           end,
                                           lists:enumerate(0, Table))))),
    SQL = io_lib:format("INSERT INTO map.~s(map_name, kind, x_position, y_position) "
                        "VALUES ~s ON CONFLICT (map_name, kind, x_position, y_position) "
                        "DO NOTHING",
                        [Name, list_to_binary(MapAttributes)]),
    do([postgres_m || 
	   _ <- {epgsql:squery(Connection, SQL), insert},
	   ok]).
