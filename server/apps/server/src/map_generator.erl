-module(map_generator).

-export([create_map/3, resource_insertificatanator/1]).
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

deresourcificate([MapName, ObjectName, I, J], Pid) ->
   UpperName = string:uppercase(ObjectName),
   case string:prefix(UpperName, "RESOURCE-") of
    nomatch -> [MapName, UpperName, I, J];
    Name -> Message = [MapName, Name, I, J], Pid ! Message, Message
   end.

resource_insertificatanator(Acc) ->
   receive
    {stop, Pid} -> Pid ! {insertificate, Acc};
    Resource -> resource_insertificatanator([io_lib:format("('~s', '~s', ~B, ~B)", Resource) | Acc])
   end.

-spec generate(epgsql:connection(), epgsql:bind_param(), {epgsql:bind_param(), list()}) -> any().
generate(Connection, MapName, {Name, Table}) ->
    % TODO: improve this garbagio
    Insertificatanator = spawn(map_generator, resource_insertificatanator, [[]]),
    MapAttributes =
        lists:join(",",
                   lists:map(fun ({I, J, ""}) ->
                                     io_lib:format("('~s', '~s', ~B, ~B)",
                                                   [MapName, "EMPTY", I, J]);
                                 ({I, J, Elem}) ->
                                     io_lib:format("('~s', '~s', ~B, ~B)",
                                                   deresourcificate([MapName, Elem, I, J], Insertificatanator))
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
        ok]),
    Insertificatanator ! {stop, self()},
    receive
      {insertificate, []} -> ok;
      {insertificate, Resources} ->
        ResourceString = list_to_binary(lists:join(",", Resources)),
        ResourceSQL = io_lib:format("INSERT INTO map.resource(map_name, kind, x_position, y_position) "
                                    "VALUES ~s;",
                                    [ResourceString]),
        do([postgres_m ||
            _ <- {epgsql:squery(Connection, ResourceSQL), insert},
            ok])
    end.
