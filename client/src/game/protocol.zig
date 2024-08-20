const messages = @import("../server_messages.zig");
const GameState = @import("../game/state.zig");
const std = @import("std");

pub fn pingUpdateCharacter(gameState: *GameState) !void {
    // TODO: We should a time out functionality (Zerl should provide one) to correctly assess
    // if we are not overwhelming the database
    try gameState.node.send(messages.Payload{
        .update_character = .{
            .name = gameState.character.stats.name,
            .x_position = gameState.character.stats.x_position,
            .y_position = gameState.character.stats.y_position,
            .map_name = gameState.character.stats.map_name,
            .username = gameState.menu.login.username[0..gameState.menu.login.usernamePosition],
            .email = gameState.menu.email,
        },
    });
    const server_response = try messages.receive_standard_response(gameState.allocator, gameState.node);
    switch (server_response) {
        .ok => {},
        .@"error" => |msg| {
            defer gameState.allocator.free(msg);
            std.debug.print("[ERROR]: {s}\n", .{msg});
            gameState.scene = .nothing;
        },
    }
}
