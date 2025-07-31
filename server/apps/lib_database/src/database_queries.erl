-module(database_queries).

-export([get_root_dir/0, fetch_query/2]).

% TODO: Comment this if debugging turns into hell
-compile({inline, [get_root_dir/0, fetch_query/2]}).

-spec from_lib_dir(Path) -> Parent
    when Path :: file:name_all(),
         Parent :: file:name_all().
from_lib_dir(Path) ->
    filename:dirname(
        filename:dirname(Path)).

-spec to_rel(Path) -> Parent
    when Path :: file:name_all(),
         Parent :: file:name_all().
to_rel(Path) ->
    Suffix = ["rel", "lyceum"],
    filename:join([Path | Suffix]).

-spec get_root_dir() -> file:name_all().
get_root_dir() ->
    LibDir =
        filename:absname(
            code:lib_dir(lib_database)),
    RootDir = from_lib_dir(LibDir),
    case application:get_env(lib_database, is_shell) of
        {ok, true} ->
            to_rel(RootDir);
        _ ->
            RootDir
    end.

-spec fetch_query(Dir, Filename) -> Content
    when Dir :: file:name_all(),
         Filename :: file:name_all(),
         Content :: binary().
fetch_query(Dir, Filename) ->
    RootDir = get_root_dir(),
    QueryDir = filename:join([RootDir, "database", "queries"]),
    Path = filename:join([QueryDir, Dir, Filename]),
    {ok, Bin} = file:read_file(Path),
    Bin.
