-module(character).

-export([create/2, player_characters/2, player_character/2, update/2,
         retrieve_near_players/2, activate/2, deactivate/4, harvest_resource/2]).

-compile({parse_transform, do}).

create(#{name := Name,
         username := Username,
         email := Email,
         constitution := Constitution,
         wisdom := Wisdom,
         strength := Strength,
         endurance := Endurance,
         intelligence := Intelligence,
         faith := Faith},
       Connection) ->
    Query = database_queries:fetch_query("character", "create_character.sql"),
    Result =
        epgsql:equery(Connection,
                      Query,
                      [Name,
                       Username,
                       Email,
                       Constitution,
                       Wisdom,
                       Strength,
                       Endurance,
                       Intelligence,
                       Faith]),
    do([postgres_m || _ <- {Result, insert}, ok]).

activate(#{name := Name,
           username := Username,
           email := Email},
         Connection) ->
    Query = database_queries:fetch_query("character", "activate_character.sql"),
    do([postgres_m
        || _ <- {epgsql:equery(Connection, Query, [Name, Email, Username]), insert}, ok]).

deactivate(Name, Email, Username, Connection) ->
    Query = database_queries:fetch_query("character", "deactivate_character.sql"),
    do([postgres_m
        || _ <- {epgsql:equery(Connection, Query, [Name, Email, Username]), delete}, ok]).

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
         mana := Mana},
       Connection) ->
    Query = database_queries:fetch_query("character", "update_character.sql"),
    Result =
        epgsql:with_transaction(Connection,
                                fun(Conn) ->
                                   epgsql:equery(Conn,
                                                 Query,
                                                 [XPosition,
                                                  YPosition,
                                                  XVelocity,
                                                  YVelocity,
                                                  Level,
                                                  Health,
                                                  Mana,
                                                  FaceDirection,
                                                  StateType,
                                                  Name,
                                                  Email,
                                                  Username,
                                                  MapName])
                                end,
                                #{begin_opts => "ISOLATION LEVEL READ UNCOMMITTED"}),
    do([postgres_m || _ <- {Result, update}, epgsql:sync(Connection)]).

retrieve_near_players(#{map_name := MapName, name := Name}, Connection) ->
    Query = database_queries:fetch_query("character", "select_nearby_characters.sql"),
    do([postgres_m
        || UnprocessedPlayers <- {epgsql:equery(Connection, Query, [MapName, Name]), select},
           ProcessedPlayers =
               database_utils:transform_character_map(
                   database_utils:columns_and_rows(UnprocessedPlayers)),
           return(ProcessedPlayers)]).

player_characters(#{username := Username, email := Email}, Connection) ->
    Query = database_queries:fetch_query("character", "select_all_characters.sql"),
    do([postgres_m
        || UnprocessedCharacters <- {epgsql:equery(Connection, Query, [Username, Email]), select},
           ColumnsRows = database_utils:columns_and_rows(UnprocessedCharacters),
           ProcessedCharacters = database_utils:transform_character_map(ColumnsRows),
           return(ProcessedCharacters)]).

player_character(#{name := Name,
                   username := Username,
                   email := Email},
                 Connection) ->
    Query = database_queries:fetch_query("character", "select_single_character.sql"),
    do([postgres_m
        || UnprocessedCharacter
               <- {epgsql:equery(Connection, Query, [Username, Email, Name]), select},
           case database_utils:transform_character_map(
                    database_utils:columns_and_rows(UnprocessedCharacter))
           of
               [C] ->
                   return(C);
               [] ->
                   fail("Updated Character not found!");
               _ ->
                   fail("Found more than one Character!")
           end]).

harvest_resource(#{name := Name,
                   username := Username,
                   email := Email,
                   map_name := MapName,
                   kind := Kind,
                   x_position := XPosition,
                   y_position := YPosition},
                 Connection) ->
    logger:debug("[~p] NAME: ~p x USERNAME: ~p~n", [?MODULE, Name, Username]),
    Harvest = database_queries:fetch_query("map", "harvest_resource.sql"),
    Inventory = database_queries:fetch_query("character", "select_inventory.sql"),
    Resource = database_queries:fetch_query("map", "select_resource_quantity.sql"),
    epgsql:with_transaction(Connection,
                            fun(Conn) ->
                               do([postgres_m
                                   || _
                                          <- {epgsql:equery(Conn,
                                                            Harvest,
                                                            [MapName,
                                                             Kind,
                                                             XPosition,
                                                             YPosition,
                                                             Name,
                                                             Email,
                                                             Username]),
                                              call},
                                      UnprocessedDeltaInventory
                                          <- {epgsql:equery(Conn,
                                                            Inventory,
                                                            [Name, Username, Email]),
                                              select},
                                      UnprocessedDeltaResource
                                          <- {epgsql:equery(Conn,
                                                            Resource,
                                                            [MapName, XPosition, YPosition, Kind]),
                                              select},
                                      DeltaInventory =
                                          hd(database_utils:columns_and_rows(UnprocessedDeltaInventory)),
                                      DeltaResource =
                                          case
                                              database_utils:columns_and_rows(UnprocessedDeltaResource)
                                          of
                                              [A] -> A;
                                              [] -> #{quantity => 0};
                                              _ -> fail("Logical contradiction.")
                                          end,
                                      return(#{delta_inventory => DeltaInventory,
                                               delta_resource =>
                                                   maps:get(quantity, DeltaResource)})])
                            end,
                            #{begin_opts => "ISOLATION LEVEL READ UNCOMMITTED"}).
