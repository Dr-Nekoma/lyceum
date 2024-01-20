-module(utilities).

-export([encode/1, decode/1, retrieve_input/1]).

-record(xml, {tag, content}).

encode(Data) ->
    lists:flatten(
        xmerl:export_simple(Data, xmerl_xml)).

decode({xmlElement,ParentName,_,_,_,_,_,_,Children,_,_,_}) ->
    io:format("HERE ELEM\n"),
    #xml{tag = ParentName,
	 content = lists:map ((fun (C) -> decode(C) end), Children)};

decode({xmlText,[{Name,_}|_],_,_,Content,text}) ->
    io:format("HERE TEXT\n"),
    #xml{ tag = Name,
	  content = Content };

decode({topLevel, Data}) ->
    io:format("HERE TOP\n"),
    {XML, []} = xmerl_scan:string(Data),
    decode(XML).


retrieve_input(Body) ->
    case Body of 
        [{Input, true}] ->
            {ok, Input};
        [] ->
            {error, cowboy_req:reply(400, #{<<"content-type">> => <<"text/xml">>}, encode([{message, [], ["Missing body on the request."]}]))};
        _ ->
            {error, cowboy_req:reply(400, #{<<"content-type">> => <<"text/xml">>}, encode([{message, [], ["Bad request."]}]))}
    end.
