-module(character).

-export([create/2, player_characters/3, update/2,retrieve_near_players/3, activate/3, deactivate/2]).

create(#{name := Name, 
	 username := Username, 
	 email := Email,
	 constitution := Constitution,
	 wisdom := Wisdom,
	 strength := Strength,
	 endurance := Endurance,
	 intelligence := Intelligence,
	 faith := Faith}, Connection) ->
    Query = "INSERT INTO character.view (name,  e_mail, username, constitution, wisdom, strength, endurance, intelligence, faith) \
             VALUES ($1::VARCHAR(18), $2::TEXT, $3::VARCHAR(32), $4::SMALLINT, $5::SMALLINT, $6::SMALLINT, $7::SMALLINT, $8::SMALLINT, $9::SMALLINT)",
    Result = epgsql:equery(Connection, Query, [Name, Username, Email, Constitution, Wisdom, Strength, Endurance, Intelligence, Faith]),
    Fun = (fun (_, _, _) -> ok end),
    util:process_postgres_result(Result, insert, Fun).

%% TODO: Add a Select first in order to check already being activated
activate(#{name := Name, 
	   username := Username, 
	   email := Email},
	   UserPid, Connection) ->
    Query = "INSERT INTO character.active (name,  e_mail, username, user_pid) \
             VALUES ($1::VARCHAR(18), $2::TEXT, $3::VARCHAR(32), $4::VARCHAR(50)) \
             ON CONFLICT (name, username, e_mail) \
             DO UPDATE SET user_pid = $4::VARCHAR(50)",
    Result = epgsql:equery(Connection, Query, [Name, Email, Username, UserPid]),
    Fun = (fun (_) -> ok end),
    util:process_postgres_result(Result, insert, Fun).

%% TODO: Add a Select first in order to check already being deactivated
deactivate(UserPid, Connection) ->
    Query = "DELETE FROM character.active \ 
             WHERE user_pid = $1::VARCHAR(50)",
    Result = epgsql:equery(Connection, Query, [UserPid]),
    Fun = (fun (_, _, _) -> ok end),
    util:process_postgres_result(Result, insert, Fun).

update(#{name := Name, 
	 username := Username, 
	 email := Email,
	 map_name := MapName,
	 face_direction := FaceDirection,
	 state_type := StateType,
	 x_position := XPosition,
	 y_position := YPosition,
	 x_velocity := XVelocity,
	 y_velocity := YVelocity}, Connection) ->
    io:format("x: ~p, y: ~p, state_type: ~p\n", [XVelocity, YVelocity, StateType]),
    Query = "UPDATE character.position SET x_position = $1::REAL, y_position = $2::REAL, \
             x_velocity = $3::REAL, y_velocity = $4::REAL, \
             face_direction = $5::SMALLINT, state_type = $6::\"character\".STATE_TYPE \ 
             WHERE name = $7::VARCHAR(18) AND e_mail = $8::TEXT AND username = $9::VARCHAR(32) AND map_name = $10::VARCHAR(64)",
    Result = epgsql:with_transaction(Connection, 
				      fun (Conn) -> epgsql:equery(Conn, Query, [XPosition, YPosition, XVelocity, YVelocity, FaceDirection, StateType, Name, Email, Username, MapName])
				      end,
				      #{ begin_opts => "ISOLATION LEVEL READ UNCOMMITTED"}),
    Fun = (fun (_) -> epgsql:sync(Connection) end),
    util:process_postgres_result(Result, update, Fun).


retrieve_near_players(#{map_name := MapName}, UserPid, Connection) ->
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
                    character.view.state_type \
             FROM character.view \
             NATURAL JOIN character.active \
             WHERE map_name = $1::VARCHAR(64) AND user_pid <> $2::VARCHAR(50)",
    Result = epgsql:equery(Connection, Query, [MapName, UserPid]),
    Fun = (fun (FullColumns, Values) -> {ok, util:transform_character_map(util:columns_and_rows(FullColumns, Values))} end),
    util:process_postgres_result(Result, select, Fun).


player_characters(Username, Email, Connection) ->
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
                    character.view.state_type \
             FROM character.view WHERE username = $1::VARCHAR(32) AND e_mail = $2::TEXT",
    Result = epgsql:equery(Connection, Query, [Username, Email]),
    Fun = (fun (FullColumns, Values) -> {ok, util:transform_character_map(util:columns_and_rows(FullColumns, Values))} end),
    util:process_postgres_result(Result, select, Fun).
    %% io:format("Username: ~p, Email: ~p, Data: ~p\n", [Username, Email, Something]),    

