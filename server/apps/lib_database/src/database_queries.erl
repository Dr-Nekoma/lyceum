-module(database_queries).

-export([get_root_dir/0, fetch_query/2]).

% This will embed queries at compile time
-define(QUERY_ROOT, "database/queries").

% TODO: Comment this if debugging turns into hell
-compile({inline, [get_root_dir/0, fetch_query/2]}).

-spec from_lib_dir(Path) -> Parent when
    Path :: file:name_all(),
    Parent :: file:name_all().
from_lib_dir(Path) ->
    filename:dirname(
        filename:dirname(Path)
    ).

-spec to_rel(Path) -> Parent when
    Path :: file:name_all(),
    Parent :: file:name_all().
to_rel(Path) ->
    Suffix = ["rel", "lyceum"],
    filename:join([Path | Suffix]).

-spec get_root_dir() -> file:name_all().
get_root_dir() ->
    LibDir =
        filename:absname(
            code:lib_dir(lib_database)
        ),
    RootDir = from_lib_dir(LibDir),
    case application:get_env(lib_database, is_shell) of
        {ok, true} ->
            to_rel(RootDir);
        _ ->
            RootDir
    end.

% Helper to read file at compile time
-spec read_embedded_query(Dir, Filename) -> Content when
    Dir :: file:name_all(),
    Filename :: file:name_all(),
    Content :: binary().
read_embedded_query(Dir, Filename) ->
    RootDir = get_root_dir(),
    QueryDir = filename:join([RootDir, ?QUERY_ROOT]),
    Path = filename:join([QueryDir, Dir, Filename]),
    {ok, Bin} = file:read_file(Path),
    Bin.

-spec fetch_query(Dir, Filename) -> Content when
    Dir :: file:name_all(),
    Filename :: file:name_all(),
    Content :: binary().
fetch_query("character" = D, "activate_character.sql" = F) ->
    read_embedded_query(D, F);
fetch_query("character" = D, "create_character.sql" = F) ->
    read_embedded_query(D, F);
fetch_query("character" = D, "deactivate_character.sql" = F) ->
    read_embedded_query(D, F);
fetch_query("character" = D, "select_all_characters.sql" = F) ->
    read_embedded_query(D, F);
fetch_query("character" = D, "select_inventory.sql" = F) ->
    read_embedded_query(D, F);
fetch_query("character" = D, "select_nearby_characters.sql" = F) ->
    read_embedded_query(D, F);
fetch_query("character" = D, "select_single_character.sql" = F) ->
    read_embedded_query(D, F);
fetch_query("character" = D, "update_character.sql" = F) ->
    read_embedded_query(D, F);
fetch_query("map" = D, "select_map_dimensions.sql" = F) ->
    read_embedded_query(D, F);
fetch_query("map" = D, "select_map_objects.sql" = F) ->
    read_embedded_query(D, F);
fetch_query("map" = D, "select_map_resources.sql" = F) ->
    read_embedded_query(D, F);
fetch_query("map" = D, "harvest_resource.sql" = F) ->
    read_embedded_query(D, F);
fetch_query("map" = D, "select_resource_quantity.sql" = F) ->
    read_embedded_query(D, F);
fetch_query("map" = D, "select_tiles.sql" = F) ->
    read_embedded_query(D, F).
