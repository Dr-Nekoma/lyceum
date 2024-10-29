-module(generate_map).

-export([generate/2, fetch_file/1]).

fetch_file(CsvPath) ->
    {ok, Bin} = file:read_file(CsvPath),
    Content = csv:decode_binary(Bin),
    {case filename:basename(CsvPath) of "tile.csv" -> "tile"; "object.csv" -> "object" end, Content}.

generate(Conn, {Name, Table}) ->
    X = lists:join(",", lists:map(fun ({I, J, ""}) -> io_lib:format("('road', '~s', ~B, ~B)", ["EMPTY", I, J]); ({I, J, Elem}) -> io_lib:format("('road', '~s', ~B, ~B)", [string:uppercase(Elem), I, J]) end, lists:flatten(lists:map(fun ({I, Out}) -> lists:map(fun ({J, In}) -> {I, J, In} end, lists:enumerate(0, Out)) end, lists:enumerate(0, Table))))),
    SQL = io_lib:format("INSERT INTO map.~s(map_name, kind, x_position, y_position) VALUES ", [Name]),
    GARBAGE = string:concat(SQL, list_to_binary(X)),
    io:format("~s", [GARBAGE]),
    epgsql:squery(Conn, GARBAGE).

