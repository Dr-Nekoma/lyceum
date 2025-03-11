const assets = @import("../assets.zig");
const config = @import("../config.zig");
const messages = @import("messages.zig");
const rl = @import("raylib");
const std = @import("std");
const GameState = @import("../game/state.zig");
const GameCharacter = @import("../game/character.zig");

pub const character = struct {
    fn updatePhysicsStats(player: *GameCharacter, stats: messages.Character.Info) void {
        player.position = .{
            .x = stats.x_position,
            .y = player.position.y,
            .z = stats.y_position,
        };
        player.velocity = .{
            .x = stats.x_velocity,
            .y = player.velocity.y,
            .z = stats.y_velocity,
        };
    }

    fn updateCharacterInfo(player: *GameCharacter, stats: messages.Character.Info) void {
        player.stats.x_position = stats.x_position;
        player.stats.y_position = stats.y_position;
        player.stats.x_velocity = 0;
        player.stats.y_velocity = 0;
        player.stats.state_type = .idle;
        player.stats.face_direction = stats.face_direction;
        player.stats.constitution = stats.constitution;
        player.stats.wisdom = stats.wisdom;
        player.stats.strength = stats.strength;
        player.stats.endurance = stats.endurance;
        player.stats.intelligence = stats.intelligence;
        player.stats.faith = stats.faith;
        player.stats.map_name = stats.map_name;
        updatePhysicsStats(player, stats);
    }

    pub fn update(gameState: *GameState) !void {
        // TODO: We should a time out functionality (Zerl should provide one) to correctly assess
        // if we are not overwhelming the database
        gameState.send(messages.Payload{
            .update_character = .{
                .name = gameState.world.character.stats.name,
                .x_position = gameState.world.character.stats.x_position,
                .y_position = gameState.world.character.stats.y_position,
                .x_velocity = gameState.world.character.stats.x_velocity,
                .y_velocity = gameState.world.character.stats.y_velocity,
                .health = gameState.world.character.stats.health,
                .mana = gameState.world.character.stats.mana,
                .level = gameState.world.character.stats.level,
                .map_name = gameState.world.character.stats.map_name,
                .face_direction = gameState.world.character.stats.face_direction,
                .state_type = gameState.world.character.stats.state_type,
                .username = gameState.menu.login.username[0..gameState.menu.login.usernamePosition],
                .email = gameState.menu.login.email,
            },
        }) catch {
            gameState.errorElem.update(.update_character_send);
            return;
        };
        const node = gameState.connection.node;
        const server_response = node.receive(messages.Character.Many.Response, gameState.allocator) catch {
            gameState.errorElem.update(.update_character_receive);
            return;
        };
        switch (server_response) {
            .ok => |players| {
                const other_players = &gameState.world.other_players;
                for (players) |player| {
                    if (other_players.get(player.name)) |current_player| {
                        var next_player = current_player;
                        next_player.stats = player;
                        updatePhysicsStats(&next_player, player);
                        other_players.putAssumeCapacity(player.name, next_player);
                    } else {
                        var new_character = GameCharacter{
                            .stats = player,
                            .model = try assets.model(config.assets.paths.game.character.walker),
                            .animation = .{
                                // We can do this because all players use the same model + animations for now
                                .frames = gameState.world.character.animation.frames,
                            },
                        };
                        updatePhysicsStats(&new_character, player);
                        try other_players.put(player.name, new_character);
                    }
                }
                var present_player_iterator = other_players.valueIterator();
                while (present_player_iterator.next()) |present_player| {
                    var keep = false;
                    for (players) |player| {
                        if (std.mem.eql(u8, player.name, present_player.stats.name)) {
                            keep = true;
                            break;
                        }
                    }
                    if (!keep) {
                        _ = other_players.remove(present_player.stats.name);
                    }
                }
            },
            .@"error" => |msg| {
                defer gameState.allocator.free(msg);
                std.debug.print("[ERROR]: {s}\n", .{msg});
                return error.update_character;
            },
        }
    }
    fn readMap(gameState: *GameState, map: *const messages.World.Map) !void {
        defer gameState.allocator.free(map.resources);
        gameState.world.map.resources.clearRetainingCapacity();
        gameState.world.map.instance = map.*;
        gameState.world.map.instance.resources = &.{};
        for (map.resources) |resource| {
            const position, const payload = resource;
            try gameState.world.map.resources.put(position, payload);
        }
        gameState.world.character.inventory.hud.minimap.map = try assets.createMapImage(&gameState.world.map);
    }

    pub fn joinMap(gameState: *GameState) !void {
        // TODO: We should a time out functionality (Zerl should provide one) to correctly assess
        // if we are not overwhelming the database
        gameState.send(messages.Payload{
            .joining_map = .{
                .name = gameState.world.character.stats.name,
                .username = gameState.menu.login.username[0..gameState.menu.login.usernamePosition],
                .email = gameState.menu.login.email,
                .map_name = gameState.world.character.stats.map_name,
            },
        }) catch {
            gameState.errorElem.update(.joining_map_send);
            return;
        };
        const node = gameState.connection.node;
        const server_response = node.receive(messages.Character.Join.Response, gameState.allocator) catch {
            gameState.errorElem.update(.joining_map_receive);
            return;
        };
        switch (server_response) {
            .ok => |info| {
                updateCharacterInfo(&gameState.world.character, info.character);
                try readMap(gameState, &info.map);
                gameState.scene = .spawn;
            },
            .@"error" => |msg| {
                defer gameState.allocator.free(msg);
                std.debug.print("[ERROR]: {s}\n", .{msg});
                return error.joining_map;
            },
        }
    }

    pub fn harvestResource(gameState: *GameState, resource: messages.World.Resource, position: messages.World.Position) !void {
        // TODO: We should a time out functionality (Zerl should provide one) to correctly assess
        // if we are not overwhelming the database
        const x, const y = position;
        gameState.send(messages.Payload{
            .harvest_resource = .{
                .name = gameState.world.character.stats.name,
                .username = gameState.menu.login.username[0..gameState.menu.login.usernamePosition],
                .email = gameState.menu.login.email,
                .map_name = gameState.world.character.stats.map_name,
                .kind = resource.kind,
                .x_position = x,
                .y_position = y,
            },
        }) catch {
            gameState.errorElem.update(.harvest_resource_send);
            return;
        };
        const node = gameState.connection.node;
        const server_response = node.receive(messages.Character.Harvest.Response, gameState.allocator) catch {
            gameState.errorElem.update(.harvest_resource_receive);
            return;
        };
        switch (server_response) {
            .ok => |_| {
                std.debug.print("We should update the inventory and the rendered map", .{});
            },
            .@"error" => |msg| {
                defer gameState.allocator.free(msg);
                std.debug.print("[ERROR]: {s}\n", .{msg});
                return error.harvest_resource;
            },
        }
    }

    pub fn exitMap(gameState: *GameState) !void {
        // TODO: We should a time out functionality (Zerl should provide one) to correctly assess
        // if we are not overwhelming the database
        gameState.send(messages.Payload.exit_map) catch {
            gameState.errorElem.update(.exit_send);
            return;
        };
        const node = gameState.connection.node;
        const server_response = node.receive(messages.ErlangResponse, gameState.allocator) catch {
            gameState.errorElem.update(.exit_receive);
            return;
        };
        switch (server_response) {
            .ok => {
                gameState.scene = .nothing;
            },
            .@"error" => |msg| {
                defer gameState.allocator.free(msg);
                std.debug.print("[ERROR]: {s}\n", .{msg});
                return error.exit;
            },
        }
    }
};

pub const user = struct {
    pub fn login(gameState: *GameState) !void {
        gameState.send_with_self(.{
            .login = .{
                .username = gameState.menu.login.username[0..gameState.menu.login.usernamePosition],
                .password = gameState.menu.login.password[0..gameState.menu.login.passwordPosition],
            },
        }) catch {
            gameState.errorElem.update(.login_send);
            return;
        };
        const node = gameState.connection.node;

        const server_response = node.receive(messages.User.Login.Response, gameState.allocator) catch {
            gameState.errorElem.update(.login_receive);
            return;
        };
        switch (server_response) {
            .ok => |item| {
                gameState.connection.handler, gameState.menu.login.email = item;
                try getCharacters(gameState);
            },
            .@"error" => |msg| {
                defer gameState.allocator.free(msg);
                std.debug.print("[ERROR]: {s}\n", .{msg});
                gameState.errorElem.update(.login_invalid);
                return;
            },
        }
    }

    pub fn logout(gameState: *GameState) void {
        if (gameState.connection.handler != null) {
            gameState.send(messages.Payload.logout) catch {
                gameState.errorElem.update(.logout_send);
                return;
            };
            const node = gameState.connection.node;
            const server_response = node.receive(messages.ErlangResponse, gameState.allocator) catch {
                gameState.errorElem.update(.logout_receive);
                return;
            };
            switch (server_response) {
                .ok => {
                    gameState.connection.handler = null;
                    gameState.scene = .nothing;
                },
                .@"error" => |msg| {
                    defer gameState.allocator.free(msg);
                    std.debug.print("[ERROR]: {s}\n", .{msg});
                    gameState.errorElem.update(.logout_invalid);
                    return;
                },
            }
        }
    }

    pub fn getCharacters(gameState: *GameState) !void {
        gameState.send(messages.Payload{
            .list_characters = .{
                .username = gameState.menu.login.username[0..gameState.menu.login.usernamePosition],
                .email = gameState.menu.login.email,
            },
        }) catch {
            gameState.errorElem.update(.get_characters_send);
            return;
        };

        const node = gameState.connection.node;
        const maybe_characters = node.receive(messages.Character.Many.Response, gameState.allocator) catch {
            gameState.errorElem.update(.get_characters_receive);
            return;
        };
        switch (maybe_characters) {
            .ok => |erlang_characters| {
                const placeholder = try assets.texture(config.assets.paths.menu.character.placeholder);

                var characters = std.ArrayList(GameCharacter).init(gameState.allocator);
                for (erlang_characters) |stats| {
                    try characters.append(.{
                        .stats = stats,
                        .preview = placeholder,
                    });
                }
                gameState.menu.character.select.list = characters.items;
                gameState.scene = .character_selection;
            },
            .@"error" => |error_msg| {
                std.debug.print("error in server: {s}", .{error_msg});
                gameState.errorElem.update(.get_characters_invalid);
                gameState.scene = .nothing;
            },
        }
    }
};
