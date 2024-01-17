-module(utilities).

-export([encode/1, decode/1, retrieve_input/1]).

encode(Data) ->
    lists:flatten(
        xmerl:export_simple(Data, xmerl_xml)).

decode(Data) ->
    {{_,_,_,_,_,_,_,_,XML,_,_,_},_} = xmerl_scan:string(Data),
    XML.

retrieve_input(Body) ->
    case Body of 
        [{Input, true}] ->
            {ok, Input};
        [] ->
            {error, cowboy_req:reply(400, #{<<"content-type">> => <<"text/xml">>}, encode([{message, [], ["Missing body on the request."]}]))};
        _ ->
            {error, cowboy_req:reply(400, #{<<"content-type">> => <<"text/xml">>}, encode([{message, [], ["Bad request."]}]))}
    end.
