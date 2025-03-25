-module(map_generator).

-export([create_map/3, resource_inserter/1]).
-compile({parse_transform, do}).

create_assets(Connection, MapPath, MapName, FileName) ->
    AssetPath = filename:join([MapPath, MapName, FileName]),
    Assets = fetch_file(AssetPath),
    generate(Connection, MapName, Assets).

create_map(Connection, MapPath, MapName) ->
    create_assets(Connection, MapPath, MapName, "tile.csv"),
    create_assets(Connection, MapPath, MapName, "object.csv").

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

remove_resource_prefix([MapName, ObjectName, I, J], Pid) ->
   UpperName = string:uppercase(ObjectName),
   case string:prefix(UpperName, "RESOURCE-") of
    nomatch -> [MapName, UpperName, I, J];
    Name -> Message = [MapName, Name, I, J], Pid ! Message, Message
   end.

resource_inserter(Acc) ->
   receive
    {stop, Pid} -> Pid ! {insert, Acc};
    Resource -> resource_inserter([io_lib:format("('~s', '~s', ~B, ~B)", Resource) | Acc])
   end.

-spec generate(epgsql:connection(), epgsql:bind_param(), {epgsql:bind_param(), list()}) -> any().
generate(Connection, MapName, {Name, Table}) ->
    % TODO: improve this garbagio
    Inserter = spawn(map_generator, resource_inserter, [[]]),
    MapAttributes =
        lists:join(",",
                   lists:map(fun ({I, J, ""}) ->
                                     io_lib:format("('~s', '~s', ~B, ~B)",
                                                   [MapName, "EMPTY", I, J]);
                                 ({I, J, Elem}) ->
                                     io_lib:format("('~s', '~s', ~B, ~B)",
                                                   remove_resource_prefix([MapName, Elem, I, J], Inserter))
                             end,
                             lists:flatten(
                                 lists:map(fun({J, Out}) ->
                                              lists:map(fun({I, In}) -> {I, J, In} end,
                                                        lists:enumerate(0, Out))
                                           end,
                                           lists:enumerate(0, Table))))),
    SQL = io_lib:format("INSERT INTO map.~s(map_name, kind, x_position, y_position) "
                        "VALUES ~s ON CONFLICT (map_name, kind, x_position, y_position) "
                        "DO NOTHING",
                        [Name, list_to_binary(MapAttributes)]),
    do([postgres_m ||
        _ <- {epgsql:squery(Connection, SQL), insert},
        ok]),
    Inserter ! {stop, self()},
    receive
      {insert, []} -> ok;
      {insert, Resources} ->
        ResourceString = list_to_binary(lists:join(",", Resources)),
        ResourceSQL = io_lib:format("INSERT INTO map.resource(map_name, kind, x_position, y_position) "
                                    "VALUES ~s;",
                                    [ResourceString]),
        do([postgres_m ||
            _ <- {epgsql:squery(Connection, ResourceSQL), insert},
            ok])
    end.
