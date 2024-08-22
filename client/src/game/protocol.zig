const messages = @import("../server_messages.zig");
const GameState = @import("../game/state.zig");
const std = @import("std");

pub fn pingUpdateCharacter(gameState: *GameState) !void {
    // TODO: We should a time out functionality (Zerl should provide one) to correctly assess
    // if we are not overwhelming the database
    if (gameState.node) |*nod| {
        try nod.send(messages.Payload{
            .update_character = .{
                .name = gameState.character.stats.name,
                .x_position = gameState.character.stats.x_position,
                .y_position = gameState.character.stats.y_position,
                .map_name = gameState.character.stats.map_name,
                .face_direction = gameState.character.stats.face_direction,
                .username = gameState.menu.login.username[0..gameState.menu.login.usernamePosition],
                .email = gameState.menu.email,
            },
        });
        const server_response = try messages.receive_characters_list(gameState.allocator, nod);
        switch (server_response) {
            .ok => |players| {
                gameState.other_players = players;
            },
            .@"error" => |msg| {
                defer gameState.allocator.free(msg);
                std.debug.print("[ERROR]: {s}\n", .{msg});
                gameState.scene = .nothing;
            },
        }
    }
}
