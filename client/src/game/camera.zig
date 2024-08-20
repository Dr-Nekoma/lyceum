const rl = @import("raylib");
const rm = rl.math;
const config = @import("../config.zig");
const GameState = @import("../game/state.zig");

pub fn update(gameState: *GameState) void {
    gameState.camera.target = gameState.character.position;

    const cameraAngle = rm.vector3Normalize(config.angleCameraVector);

    gameState.cameraDistance -= rl.getMouseWheelMove() * 2;

    gameState.camera.position = rm.vector3Add(rm.vector3Scale(cameraAngle, gameState.cameraDistance), gameState.character.position);
    rl.updateCamera(&gameState.camera, rl.CameraMode.camera_first_person);
}
