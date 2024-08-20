-module(character).

-export([create/2, player_characters/3, update/2, updateTemp/2, retrieve/2]).

create(#{name := Name, 
	 username := Username, 
	 email := Email,
	 constitution := Constitution,
	 wisdom := Wisdom,
	 strength := Strength,
	 endurance := Endurance,
	 intelligence := Intelligence,
	 faith := Faith}, Connection) ->
    Query = "INSERT INTO lyceum.view_character (name,  e-mail, username, constitution, wisdom, strength, endurance, intelligence, faith) \
             VALUES ($1::VARCHAR(18), $2::TEXT, $3::VARCHAR(32), $4::SMALLINT, $5::SMALLINT, $6::SMALLINT, $7::SMALLINT, $8::SMALLINT, $9::SMALLINT)",
    {ok, _, _} = epgsql:equery(Connection, Query, [Name, Username, Email, Constitution, Wisdom, Strength, Endurance, Intelligence, Faith]).

update(#{name := Name, 
	 username := Username, 
	 email := Email,
	 map_name := MapName,
	 x_position := XPosition,
	 y_position := YPosition}, Connection) ->
    Query = "UPDATE lyceum.view_character SET x_position = $1::SMALLINT, y_position = $2::SMALLINT \ 
             WHERE name = $3::VARCHAR(18) AND e_mail = $4::TEXT AND username = $5::VARCHAR(32) AND map_name = $6::VARCHAR(64)",
    {ok, _} = epgsql:equery(Connection, Query, [XPosition, YPosition, Name, Email, Username, MapName]).

updateTemp(#{name := Name, 
	     username := Username, 
	     email := Email,
	     map_name := MapName,
	     x_position := XPosition,
	     y_position := YPosition}, Connection) ->
    io:format("x: ~p, y: ~p", [XPosition, YPosition]),
    Query = "UPDATE lyceum.character_position SET x_position = $1::SMALLINT, y_position = $2::SMALLINT \ 
             WHERE name = $3::VARCHAR(18) AND e_mail = $4::TEXT AND username = $5::VARCHAR(32) AND map_name = $6::VARCHAR(64)",
    {ok, _} = epgsql:equery(Connection, Query, [XPosition, YPosition, Name, Email, Username, MapName]).

retrieve(#{name := Name, 
	   username := Username, 
	   email := Email,
	   map_name := MapName}, Connection) ->
    Query = "SELECT lyceum.view_character.x_position, \
                    lyceum.view_character.y_position  \
             WHERE name = $1::VARCHAR(18), e_mail = $2::TEXT, username = $3::VARCHAR(32), map_name = $4::VARCHAR(64)",
    {ok, FullColumns, Values} = epgsql:equery(Connection, Query, [Name, Email, Username, MapName]),
    util:columns_and_rows(FullColumns, Values).

player_characters(Username, Email, Connection) ->
    Query = "SELECT lyceum.view_character.name, \
                    lyceum.view_character.constitution, \
                    lyceum.view_character.wisdom, \
                    lyceum.view_character.strength, \
                    lyceum.view_character.endurance, \
                    lyceum.view_character.intelligence, \
                    lyceum.view_character.faith, \
                    lyceum.view_character.x_position, \
                    lyceum.view_character.y_position, \
                    lyceum.view_character.map_name \
             FROM lyceum.view_character WHERE username = $1::VARCHAR(32) AND e_mail = $2::TEXT",
    {ok, FullColumns, Values} = epgsql:equery(Connection, Query, [Username, Email]),
    util:columns_and_rows(FullColumns, Values).


%% INSERT INTO lyceum."view_character"("name", "e-mail", "username", "constitution", "wisdom", "strength", "endurance", "intelligence", "faith")
%% VALUES ('knight', 'test@email.com', 'test', 10, 12, 13, 14, 15, 16);


