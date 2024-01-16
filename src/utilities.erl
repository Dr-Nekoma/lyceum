-module(utilities).

-export([encode/1]).

encode(Data) ->
    lists:flatten(
        xmerl:export_simple(Data, xmerl_xml)).
