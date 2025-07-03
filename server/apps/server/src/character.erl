-module(character).

-export([create/2, player_characters/2, player_character/2, update/2, retrieve_near_players/2, activate/2, deactivate/4, harvest_resource/2]).
-compile({parse_transform, do}).

create(#{name := Name, 
	 username := Username, 
	 email := Email,
	 constitution := Constitution,
	 wisdom := Wisdom,
	 strength := Strength,
	 endurance := Endurance,
	 intelligence := Intelligence,
	 faith := Faith}, Connection) ->
    Query = "INSERT INTO character.view (name, email, username, constitution, wisdom, strength, endurance, intelligence, faith) \
             VALUES ($1::TEXT, $2::TEXT, $3::TEXT, $4::SMALLINT, $5::SMALLINT, $6::SMALLINT, $7::SMALLINT, $8::SMALLINT, $9::SMALLINT)",
    Result = epgsql:equery(Connection, Query, [Name, Username, Email, Constitution, Wisdom, Strength, Endurance, Intelligence, Faith]),
    do([postgres_m || 
	   _ <- {Result, insert},
	   ok]).

%% TODO: Add a Select first in order to check already being activated
activate(#{name := Name, 
	   username := Username, 
	   email := Email},
	   Connection) ->
    Query = "INSERT INTO character.active (name, email, username) \
             VALUES ($1::TEXT, $2::TEXT, $3::TEXT)",
    do([postgres_m || 
	   _ <- {epgsql:equery(Connection, Query, [Name, Email, Username]), insert},
	   ok]).

%% TODO: Add a Select first in order to check already being deactivated
deactivate(Name, Email, Username, Connection) ->
    Query = "DELETE FROM character.active \ 
             WHERE name = $1::TEXT AND email = $2::TEXT AND username = $3::TEXT",
    do([postgres_m || 
	   _ <- {epgsql:equery(Connection, Query, [Name, Email, Username]), delete},
	   ok]).

update(#{name := Name, 
	 username := Username, 
	 email := Email,
	 map_name := MapName,
	 face_direction := FaceDirection,
	 state_type := StateType,
	 x_position := XPosition,
	 y_position := YPosition,
	 x_velocity := XVelocity,
	 y_velocity := YVelocity,
	 level := Level,
	 health := Health,
	 mana := Mana}, Connection) ->
    Query = "UPDATE character.view SET x_position = $1::REAL, y_position = $2::REAL, \
             x_velocity = $3::REAL, y_velocity = $4::REAL, level = $5::SMALLINT, health = $6::SMALLINT, mana = $7::SMALLINT, \
             face_direction = $8::SMALLINT, state_type = $9::\"character\".STATE_TYPE \ 
             WHERE name = $10::TEXT AND email = $11::TEXT AND username = $12::TEXT AND map_name = $13::TEXT",
    Result = epgsql:with_transaction(Connection, 
				      fun (Conn) -> 
					      epgsql:equery(Conn, Query, 
							    [XPosition, YPosition, XVelocity, YVelocity, Level, Health, Mana, FaceDirection, StateType, Name, Email, Username, MapName])
				      end,
				      #{ begin_opts => "ISOLATION LEVEL READ UNCOMMITTED"}),
    do([postgres_m || 
	   _ <- {Result, update},
	   epgsql:sync(Connection)]).

retrieve_near_players(#{map_name := MapName, name := Name}, Connection) ->
    Query = "SELECT character.view.name, \
                    character.view.constitution, \
                    character.view.wisdom, \
                    character.view.strength, \
                    character.view.endurance, \
                    character.view.intelligence, \
                    character.view.faith, \
                    character.view.x_position, \
                    character.view.y_position, \
                    character.view.x_velocity, \
                    character.view.y_velocity, \
                    character.view.map_name, \
                    character.view.face_direction, \
                    character.view.level, \
                    character.view.health_max, \
                    character.view.health, \
                    character.view.mana_max, \
                    character.view.mana, \
                    character.view.state_type \
             FROM character.view \
             NATURAL JOIN character.active \
             WHERE map_name = $1::TEXT AND name <> $2::TEXT",
    do([postgres_m || 
	   UnprocessedPlayers <- {epgsql:equery(Connection, Query, [MapName, Name]), select},
	   ProcessedPlayers = database_utils:transform_character_map(database_utils:columns_and_rows(UnprocessedPlayers)),
	   return(ProcessedPlayers)]).

player_characters(#{username := Username, email := Email}, Connection) ->
    Query = "SELECT character.view.name, \
                    character.view.constitution, \
                    character.view.wisdom, \
                    character.view.strength, \
                    character.view.endurance, \
                    character.view.intelligence, \
                    character.view.faith, \
                    character.view.x_position, \
                    character.view.y_position, \
                    character.view.x_velocity, \
                    character.view.y_velocity, \
                    character.view.map_name, \
                    character.view.face_direction, \
                    character.view.level, \
                    character.view.health_max, \
                    character.view.health, \
                    character.view.mana_max, \
                    character.view.mana, \
                    character.view.state_type \
             FROM character.view WHERE username = $1::TEXT AND email::TEXT = $2::TEXT",
    do([postgres_m || 
	   UnprocessedCharacters <- {epgsql:equery(Connection, Query, [Username, Email]), select},
	   ProcessedCharacters = database_utils:transform_character_map(database_utils:columns_and_rows(UnprocessedCharacters)),
	   return(ProcessedCharacters)]).

player_character(#{name := Name, 
		   username := Username, 
		   email := Email}, Connection) ->
    Query = "SELECT character.view.constitution, \
                    character.view.wisdom, \
                    character.view.strength, \
                    character.view.endurance, \
                    character.view.intelligence, \
                    character.view.faith, \
                    character.view.x_position, \
                    character.view.y_position, \
                    character.view.map_name, \
                    character.view.face_direction, \
                    character.view.level, \
                    character.view.health_max, \
                    character.view.health, \
                    character.view.mana_max, \
                    character.view.mana, \
                    character.view.state_type \
             FROM character.view WHERE username = $1::TEXT AND email = $2::TEXT AND name = $3::TEXT",
    do([postgres_m || 
	   UnprocessedCharacter <- {epgsql:equery(Connection, Query, [Username, Email, Name]), select},
	   case database_utils:transform_character_map(database_utils:columns_and_rows(UnprocessedCharacter)) of
	       [C|[]] -> return(C);
	       [] -> fail("Updated Character not found!");
	       _ -> fail("Found more than one Character!")
	   end]).

harvest_resource(#{name := Name,
		   username := Username,
		   email := Email,
		   map_name := MapName,
		   kind := Kind,
		   x_position := XPosition,
		   y_position := YPosition},
		 Connection) ->
    logger:info("NAME: ~p USERNAME: ~p~n", [Name, Username]),
    Harvest = "CALL map.harvest_resource($1::TEXT, $2::\"map\".OBJECT_TYPE, $3::REAL, $4::REAL, $5::TEXT, $6::TEXT, $7::TEXT);",

    Inventory = "SELECT item_name, quantity FROM map.resource_item_view WHERE\
                  name = $1::TEXT AND username = $2::TEXT AND email = $3::TEXT\
                 LIMIT 1",
    Resource = "SELECT quantity FROM map.resource WHERE map_name = $1::TEXT AND x_position = $2::REAL AND y_position = $3::REAL AND kind = $4::\"map\".OBJECT_TYPE LIMIT 1",
    epgsql:with_transaction(Connection,
				      fun (Conn) ->
                         do([postgres_m ||
				_ <- {epgsql:equery(Conn, Harvest, [MapName, Kind, XPosition, YPosition, Name, Email, Username]), call},
				UnprocessedDeltaInventory <- {epgsql:equery(Conn, Inventory, [Name, Username, Email]), select},
				UnprocessedDeltaResource <- {epgsql:equery(Conn, Resource, [MapName, XPosition, YPosition, Kind]), select},
				DeltaInventory = hd(database_utils:columns_and_rows(UnprocessedDeltaInventory)),
				DeltaResource = case database_utils:columns_and_rows(UnprocessedDeltaResource) of
                         [A] -> A;
                         [] -> #{quantity => 0};
                         _ -> fail("Logical contradiction.") end,
				return(#{delta_inventory => DeltaInventory, delta_resource => maps:get(quantity, DeltaResource)})])
				      end,
			    #{ begin_opts => "ISOLATION LEVEL READ UNCOMMITTED"}).
