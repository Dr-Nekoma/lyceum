const config = @import("../config.zig");
const rl = @import("raylib");
const rm = rl.math;
const GameState = @import("../game/state.zig");

pub fn update(gameState: *GameState) void {
    gameState.world.camera.target = gameState.world.character.position;

    const cameraAngle = rm.vector3Normalize(config.angleCameraVector);

    gameState.world.cameraDistance -= rl.getMouseWheelMove() * 2;

    gameState.world.camera.position = rm.vector3Add(rm.vector3Scale(cameraAngle, gameState.world.cameraDistance), gameState.world.character.position);
    rl.updateCamera(&gameState.world.camera, .custom);
}
