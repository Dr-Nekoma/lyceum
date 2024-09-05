const camera = @import("../game/camera.zig");
const messages = @import("../server_messages.zig");
const physics = @import("../game/physics.zig");
const protocol = @import("../game/protocol.zig");
const rl = @import("raylib");
const GameState = @import("../game/state.zig");

fn drawPlayers(gameState: *GameState) void {
    for (gameState.world.other_players) |player| {
        const maybeModel = gameState.world.character.model;
        if (maybeModel) |model| {
            const position: rl.Vector3 = .{
                .x = @floatFromInt(player.x_position),
                .y = physics.character.floorLevel,
                .z = @floatFromInt(player.y_position),
            };
            rl.drawModelEx(model, position, physics.heightAxis, @floatFromInt(player.face_direction), physics.character.modelScale, rl.Color.white);
        }
    }
}

pub fn spawn(gameState: *GameState) !void {
    rl.beginMode3D(gameState.world.camera);
    defer rl.endMode3D();

    physics.character.draw(gameState);
    camera.update(gameState);
    rl.drawGrid(20, 10.0);
    try protocol.pingUpdateCharacter(gameState);
    drawPlayers(gameState);
    if (rl.isKeyDown(.key_q)) {
        try protocol.pingExitMap(gameState);
        gameState.scene = .nothing;
    }
}
