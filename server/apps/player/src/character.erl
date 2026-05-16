-module(character).

-export([
    create/2,
    player_characters/2,
    player_character/2,
    update/2,
    retrieve_near_players/2,
    activate/2,
    deactivate/4,
    harvest_resource/2
]).

-compile({parse_transform, do}).

create(
    #{
        name := Name,
        username := Username,
        email := Email,
        constitution := Constitution,
        wisdom := Wisdom,
        strength := Strength,
        endurance := Endurance,
        intelligence := Intelligence,
        faith := Faith
    },
    Pool
) ->
    Query = database_queries:fetch_query("character", "create_character.sql"),
    Result =
        database:query(
            Pool,
            Query,
            [
                Name,
                Username,
                Email,
                Constitution,
                Wisdom,
                Strength,
                Endurance,
                Intelligence,
                Faith
            ]
        ),
    do([postgres_m || _ <- Result, ok]).

activate(
    #{
        name := Name,
        username := Username,
        email := Email
    },
    Pool
) ->
    Query = database_queries:fetch_query("character", "activate_character.sql"),
    do([
        postgres_m
     || _ <- database:query(Pool, Query, [Name, Email, Username]), ok
    ]).

deactivate(Name, Email, Username, Pool) ->
    Query = database_queries:fetch_query("character", "deactivate_character.sql"),
    do([
        postgres_m
     || _ <- database:query(Pool, Query, [Name, Email, Username]), ok
    ]).

update(
    #{
        name := Name,
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
        mana := Mana
    },
    Pool
) ->
    Query = database_queries:fetch_query("character", "update_character.sql"),
    database:transaction(
        Pool,
        fun() ->
            do([
                postgres_m
             || _ <- database:query(
                        Pool,
                        <<"SET TRANSACTION ISOLATION LEVEL READ UNCOMMITTED">>,
                        []
                    ),
                _ <- database:query(
                        Pool,
                        Query,
                        [
                            XPosition,
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
                            MapName
                        ]
                    ),
                ok
            ])
        end
    ).

retrieve_near_players(#{map_name := MapName, name := Name}, Pool) ->
    Query = database_queries:fetch_query("character", "select_nearby_characters.sql"),
    do([
        postgres_m
     || UnprocessedPlayers <- database:query(Pool, Query, [MapName, Name]),
        ProcessedPlayers =
            database_utils:transform_character_map(
                database_utils:columns_and_rows(UnprocessedPlayers)
            ),
        return(ProcessedPlayers)
    ]).

player_characters(#{username := Username, email := Email}, Pool) ->
    Query = database_queries:fetch_query("character", "select_all_characters.sql"),
    do([
        postgres_m
     || UnprocessedCharacters <- database:query(Pool, Query, [Username, Email]),
        Rows = database_utils:columns_and_rows(UnprocessedCharacters),
        ProcessedCharacters = database_utils:transform_character_map(Rows),
        return(ProcessedCharacters)
    ]).

player_character(
    #{
        name := Name,
        username := Username,
        email := Email
    },
    Pool
) ->
    Query = database_queries:fetch_query("character", "select_single_character.sql"),
    do([
        postgres_m
     || UnprocessedCharacter <-
            database:query(Pool, Query, [Username, Email, Name]),
        case
            database_utils:transform_character_map(
                database_utils:columns_and_rows(UnprocessedCharacter)
            )
        of
            [C] ->
                return(C);
            [] ->
                fail("Updated Character not found!");
            _ ->
                fail("Found more than one Character!")
        end
    ]).

harvest_resource(
    #{
        name := Name,
        username := Username,
        email := Email,
        map_name := MapName,
        kind := Kind,
        x_position := XPosition,
        y_position := YPosition
    },
    Pool
) ->
    logger:debug("[~p] NAME: ~p x USERNAME: ~p~n", [?MODULE, Name, Username]),
    Harvest = database_queries:fetch_query("map", "harvest_resource.sql"),
    Inventory = database_queries:fetch_query("character", "select_inventory.sql"),
    Resource = database_queries:fetch_query("map", "select_resource_quantity.sql"),
    database:transaction(
        Pool,
        fun() ->
            do([
                postgres_m
             || _ <- database:query(
                        Pool,
                        <<"SET TRANSACTION ISOLATION LEVEL READ UNCOMMITTED">>,
                        []
                    ),
                _ <- database:query(
                        Pool,
                        Harvest,
                        [
                            MapName,
                            Kind,
                            XPosition,
                            YPosition,
                            Name,
                            Email,
                            Username
                        ]
                    ),
                UnprocessedDeltaInventory <-
                    database:query(Pool, Inventory, [Name, Username, Email]),
                UnprocessedDeltaResource <-
                    database:query(Pool, Resource, [MapName, XPosition, YPosition, Kind]),
                DeltaInventory =
                    hd(database_utils:columns_and_rows(UnprocessedDeltaInventory)),
                DeltaResource =
                    case database_utils:columns_and_rows(UnprocessedDeltaResource) of
                        [A] -> A;
                        [] -> #{quantity => 0};
                        _ -> fail("Logical contradiction.")
                    end,
                return(#{
                    delta_inventory => DeltaInventory,
                    delta_resource =>
                        maps:get(quantity, DeltaResource)
                })
            ])
        end
    ).
