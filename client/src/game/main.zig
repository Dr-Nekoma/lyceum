const camera = @import("../game/camera.zig");
const messages = @import("../server_messages.zig");
const physics = @import("../game/physics.zig");
const protocol = @import("../game/protocol.zig");
const rl = @import("raylib");
const GameState = @import("../game/state.zig");

fn drawPlayers(gameState: *GameState) void {
    var player_iterator = gameState.world.other_players.valueIterator();
    while (player_iterator.next()) |player| {
        const maybeModel = gameState.world.character.model;
        if (maybeModel) |model| {
            player.model = model;
            player.animation.frames = gameState.world.character.animation.frames;
            physics.character.draw(player);
        }
    }
}

pub fn spawn(gameState: *GameState) !void {
    rl.beginMode3D(gameState.world.camera);
    defer rl.endMode3D();

    physics.character.draw(&gameState.world.character);
    camera.update(gameState);
    rl.drawGrid(20, 10.0);
    try protocol.pingUpdateCharacter(gameState);
    drawPlayers(gameState);
    if (rl.isKeyDown(.key_q)) {
        try protocol.pingExitMap(gameState);
        gameState.scene = .nothing;
    }
}
