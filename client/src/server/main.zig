const assets = @import("../assets.zig");
const messages = @import("messages.zig");
const rl = @import("raylib");
const std = @import("std");
const GameState = @import("../game/state.zig");

pub const character = struct {
    fn updatePhysicsStats(player: *GameState.World.Character, stats: messages.Character_Info) void {
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
                .map_name = gameState.world.character.stats.map_name,
                .face_direction = gameState.world.character.stats.face_direction,
                .state_type = gameState.world.character.stats.state_type,
                .username = gameState.menu.credentials.username[0..gameState.menu.credentials.usernamePosition],
                .email = gameState.menu.credentials.email,
            },
        }) catch {
            gameState.errorElem.update(.update_character_send);
            return;
        };
        const node = gameState.connection.node;
        const server_response = node.receive(messages.Characters_Response, gameState.allocator) catch {
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
                        var new_character = GameState.World.Character{
                            .stats = player,
                            .model = try assets.model("walker.m3d"),
                            .animation = .{
                                // We can do this because all players use the same model + animations for now
                                .frames = gameState.world.character.animation.frames,
                            },
                        };
                        updatePhysicsStats(&new_character, player);
                        try other_players.put(player.name, new_character);
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

    pub fn joinMap(gameState: *GameState) !void {
        // TODO: We should a time out functionality (Zerl should provide one) to correctly assess
        // if we are not overwhelming the database
        gameState.send(messages.Payload{
            .joining_map = .{
                .name = gameState.world.character.stats.name,
                .username = gameState.menu.credentials.username[0..gameState.menu.credentials.usernamePosition],
                .email = gameState.menu.credentials.email,
            },
        }) catch {
            gameState.errorElem.update(.joining_map_send);
            return;
        };
        const node = gameState.connection.node;
        const server_response = node.receive(messages.Character_Join_Response, gameState.allocator) catch {
            gameState.errorElem.update(.joining_map_receive);
            return;
        };
        switch (server_response) {
            .ok => |info| {
                gameState.world.character.stats.x_position = info.x_position;
                gameState.world.character.stats.y_position = info.y_position;
                gameState.world.character.stats.x_velocity = 0;
                gameState.world.character.stats.y_velocity = 0;
                gameState.world.character.stats.state_type = .idle;
                gameState.world.character.stats.face_direction = info.face_direction;
                gameState.world.character.stats.constitution = info.constitution;
                gameState.world.character.stats.wisdom = info.wisdom;
                gameState.world.character.stats.strength = info.strength;
                gameState.world.character.stats.endurance = info.endurance;
                gameState.world.character.stats.intelligence = info.intelligence;
                gameState.world.character.stats.faith = info.faith;
                gameState.world.character.stats.map_name = info.map_name;
                updatePhysicsStats(&gameState.world.character, info);
            },
            .@"error" => |msg| {
                defer gameState.allocator.free(msg);
                std.debug.print("[ERROR]: {s}\n", .{msg});
                return error.joining_map;
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
        const server_response = node.receive(messages.Erlang_Response, gameState.allocator) catch {
            gameState.errorElem.update(.exit_receive);
            return;
        };
        switch (server_response) {
            .ok => {},
            .@"error" => |msg| {
                defer gameState.allocator.free(msg);
                std.debug.print("[ERROR]: {s}\n", .{msg});
                return error.exit;
            },
        }
    }
};

pub const user = struct {
    pub fn login(gameState: *GameState) void {
        gameState.send_with_self(.{
            .login = .{
                .username = gameState.menu.credentials.username[0..gameState.menu.credentials.usernamePosition],
                .password = gameState.menu.credentials.password[0..gameState.menu.credentials.passwordPosition],
            },
        }) catch {
            gameState.errorElem.update(.login_send);
            return;
        };
        const node = gameState.connection.node;
        const server_response = node.receive(messages.Login_Response, gameState.allocator) catch {
            gameState.errorElem.update(.login_receive);
            return;
        };
        switch (server_response) {
            .ok => |item| {
                gameState.connection.handler, gameState.menu.credentials.email = item;
                gameState.scene = .join;
            },
            .@"error" => |msg| {
                defer gameState.allocator.free(msg);
                std.debug.print("[ERROR]: {s}\n", .{msg});
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
            const server_response = node.receive(messages.Erlang_Response, gameState.allocator) catch {
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
                    return;
                },
            }
        }
    }

    pub fn join(gameState: *GameState) !void {
        try gameState.send(messages.Payload{
            .list_characters = .{
                .username = gameState.menu.credentials.username[0..gameState.menu.credentials.usernamePosition],
                .email = gameState.menu.credentials.email,
            },
        });

        const node = gameState.connection.node;
        const maybe_characters = try node.receive(messages.Characters_Response, gameState.allocator);
        switch (maybe_characters) {
            .ok => |erlang_characters| {
                // todo: discover how to make this work
                // const teapotembed = @embedfile("../assets/teapot.png");
                // const teapotloaded = rl.loadimagefrommemory(".png", teapotembed, teapotembed.len);

                const teapot = try assets.texture("teapot.png");

                var characters = std.ArrayList(GameState.World.Character).init(gameState.allocator);

                for (erlang_characters) |stats| {
                    try characters.append(.{
                        .stats = stats,
                        .preview = teapot,
                    });
                }

                gameState.menu.character.select.list = characters.items;
                gameState.scene = .character_selection;
            },
            .@"error" => |error_msg| {
                std.debug.print("error in server: {s}", .{error_msg});
                gameState.scene = .nothing;
            },
        }
    }
};
