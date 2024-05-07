%% File: person.hrl

%% TODO: Fix this bud xD
%%-----------------------------------------------------------
%% Data Type: person
%% where:
%%    name:  A string (default is undefined).
%%    age:   An integer (default is undefined).
%%    phone: A list of integers (default is []).
%%    dict:  A dictionary containing various information 
%%           about the person. 
%%           A {Key, Value} list (default is the empty list).
%%------------------------------------------------------------
-record(user_registry, {username = none, password = none, email = none}).
