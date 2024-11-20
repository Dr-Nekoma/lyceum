-module(map_generator).

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main([MapName, MapFolderPath]) ->
    {ok, Connection} = database:connect(),
    Tiles = fetch_file(string:concat(MapFolderPath, "/tile.csv")),
    generate(Connection, MapName, Tiles),
    Objects = fetch_file(string:concat(MapFolderPath, "/object.csv")),
    generate(Connection, MapName, Objects),
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
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
                        "VALUES ",
                        [Name]),
    Query = string:concat(SQL, list_to_binary(MapAttributes)),
    Result = epgsql:squery(Connection, Query),
    Fun = fun(_) -> ok end,
    database_utils:process_postgres_result(Result, insert, Fun).
