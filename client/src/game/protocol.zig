const messages = @import("../server_messages.zig");
const GameState = @import("../game/state.zig");
const std = @import("std");

pub fn pingUpdateCharacter(gameState: *GameState) !void {
    // TODO: We should a time out functionality (Zerl should provide one) to correctly assess
    // if we are not overwhelming the database
    try gameState.send(messages.Payload{
        .update_character = .{
            .name = gameState.world.character.stats.name,
            .x_position = gameState.world.character.stats.x_position,
            .y_position = gameState.world.character.stats.y_position,
            .map_name = gameState.world.character.stats.map_name,
            .face_direction = gameState.world.character.stats.face_direction,
            .username = gameState.menu.credentials.username[0..gameState.menu.credentials.usernamePosition],
            .email = gameState.menu.credentials.email,
        },
    });
    const node = gameState.connection.node;
    const server_response = try messages.receive_characters_list(gameState.allocator, node);
    switch (server_response) {
        .ok => |players| {
            gameState.world.other_players = players;
        },
        .@"error" => |msg| {
            defer gameState.allocator.free(msg);
            std.debug.print("[ERROR]: {s}\n", .{msg});
            gameState.scene = .nothing;
        },
    }
}

pub fn pingJoinMap(gameState: *GameState) !void {
    // TODO: We should a time out functionality (Zerl should provide one) to correctly assess
    // if we are not overwhelming the database
    try gameState.send(messages.Payload{
        .joining_map = .{
            .name = gameState.world.character.stats.name,
            .x_position = gameState.world.character.stats.x_position,
            .y_position = gameState.world.character.stats.y_position,
            .map_name = gameState.world.character.stats.map_name,
            .face_direction = gameState.world.character.stats.face_direction,
            .username = gameState.menu.credentials.username[0..gameState.menu.credentials.usernamePosition],
            .email = gameState.menu.credentials.email,
        },
    });
    const node = gameState.connection.node;
    const server_response = try messages.receive_standard_response(gameState.allocator, node);
    switch (server_response) {
        .ok => {},
        .@"error" => |msg| {
            defer gameState.allocator.free(msg);
            std.debug.print("[ERROR]: {s}\n", .{msg});
            gameState.scene = .nothing;
        },
    }
}

pub fn pingExitMap(gameState: *GameState) !void {
    // TODO: We should a time out functionality (Zerl should provide one) to correctly assess
    // if we are not overwhelming the database
    try gameState.send(messages.Payload.exit_map);
    const node = gameState.connection.node;
    const server_response = try messages.receive_standard_response(gameState.allocator, node);
    switch (server_response) {
        .ok => {},
        .@"error" => |msg| {
            defer gameState.allocator.free(msg);
            std.debug.print("[ERROR]: {s}\n", .{msg});
            gameState.scene = .nothing;
        },
    }
}
