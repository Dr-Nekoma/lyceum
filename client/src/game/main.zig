const messages = @import("../server_messages.zig");
const rl = @import("raylib");
const rm = rl.math;
const config = @import("../config.zig");
const Button = @import("../components/button.zig");
const text = @import("../components/text.zig");
const GameState = @import("../game/state.zig");
const menu = @import("main.zig");
const std = @import("std");
const math = std.math;

pub fn spawn(gameState: *GameState) !void {
    const rotationAxis: rl.Vector3 = .{
        .x = 0,
        .y = 1,
        .z = 0,
    };

    const modelScale: rl.Vector3 = .{
        .x = 1.75,
        .y = 1.75,
        .z = 1.75,
    };

    gameState.camera.target = gameState.model_position;

    const cameraAngle = rm.vector3Normalize(config.angleCameraVector);
    gameState.camera.position = rm.vector3Add(rm.vector3Scale(cameraAngle, gameState.cameraDistance), gameState.model_position);
    rl.updateCamera(&gameState.camera, rl.CameraMode.camera_first_person);

    gameState.cameraDistance -= rl.getMouseWheelMove() * 2;

    rl.beginMode3D(gameState.camera);

    if (gameState.test_model) |model| {
        rl.drawModelEx(model, gameState.model_position, rotationAxis, 270, modelScale, rl.Color.white);
    }

    if (rl.isKeyDown(.key_d)) gameState.model_position.z -= 2.0;
    if (rl.isKeyDown(.key_a)) gameState.model_position.z += 2.0;
    if (rl.isKeyDown(.key_w)) gameState.model_position.x -= 2.0;
    if (rl.isKeyDown(.key_s)) gameState.model_position.x += 2.0;
    if (rl.isKeyDown(.key_space)) gameState.model_position.y += 2.0;
    if (rl.isKeyDown(.key_left_control)) gameState.model_position.y -= 2.0;

    rl.drawGrid(20, 10.0);

    // Send the modifications back to source of truth
    try gameState.node.send(messages.Payload{
        .update_character = .{
            .name = gameState.current_character.name,
            .x_position = gameState.current_character.x_position,
            .y_position = gameState.current_character.y_position,
            .map_name = gameState.current_character.map_name,
            .username = gameState.menu.login.username[0..gameState.menu.login.usernamePosition],
            .email = gameState.menu.email,
        },
    });
    rl.endMode3D();
}
