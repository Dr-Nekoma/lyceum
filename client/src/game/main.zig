const rl = @import("raylib");
const GameState = @import("../game/state.zig");
const physics = @import("../game/physics.zig");
const protocol = @import("../game/protocol.zig");
const camera = @import("../game/camera.zig");

pub fn spawn(gameState: *GameState) !void {
    rl.beginMode3D(gameState.camera);
    defer rl.endMode3D();

    physics.character.move(gameState);
    camera.update(gameState);
    rl.drawGrid(20, 10.0);
    try protocol.pingUpdateCharacter(gameState);
}
