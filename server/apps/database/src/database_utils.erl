-module(database_utils).

-export([columns_and_rows/1, transform_character_map/1]).

%% pgo is configured (in database:query/3) to return rows as maps with
%% atom keys, so this helper is now a thin passthrough kept for source
%% compatibility with the existing monadic call sites.
-spec columns_and_rows([map()]) -> [map()].
columns_and_rows(Rows) when is_list(Rows) ->
    [normalize(Row) || Row <- Rows].

normalize(Row) when is_map(Row) ->
    maps:map(fun(_K, V) when is_binary(V) -> binary_to_list(V);
                (_K, V) -> V
             end,
             Row).

-spec transform_character_map([map()]) -> [map()].
transform_character_map(List) ->
    F = fun(Map) ->
           Map#{state_type :=
                    list_to_atom(maps:get(state_type, Map))}
        end,
    lists:map(F, List).
