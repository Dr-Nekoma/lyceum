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

const heightAxis: rl.Vector3 = .{
    .x = 0,
    .y = 1,
    .z = 0,
};

const modelScale: rl.Vector3 = .{
    .x = 1.75,
    .y = 1.75,
    .z = 1.75,
};

const velocityCeiling: rl.Vector3 = .{
    .x = 120,
    .y = 120,
    .z = 120,
};

const acceleration = 120;

pub fn spawn(gameState: *GameState) !void {
    gameState.camera.target = gameState.character.position;

    const cameraAngle = rm.vector3Normalize(config.angleCameraVector);

    gameState.cameraDistance -= rl.getMouseWheelMove() * 2;

    rl.beginMode3D(gameState.camera);

    var tempAngle = gameState.character.faceDirection;
    const velocity = &gameState.character.velocity;
    const deltaTime = rl.getFrameTime();
    const deltaVelocity = deltaTime * acceleration;

    if (rl.isKeyDown(.key_d)) {
        velocity.z -= deltaVelocity;
        tempAngle = 180;
    }
    if (rl.isKeyDown(.key_a)) {
        velocity.z += deltaVelocity;
        tempAngle = 0;
    }
    if (rl.isKeyDown(.key_w)) {
        velocity.x -= deltaVelocity;
        tempAngle = 270;
    }
    if (rl.isKeyDown(.key_s)) {
        velocity.x += deltaVelocity;
        tempAngle = 90;
    }
    if (rl.isKeyDown(.key_space)) {
        velocity.y += deltaVelocity;
    }
    if (rl.isKeyDown(.key_left_control)) {
        velocity.y -= deltaVelocity;
    }

    const frictionFactor = deltaVelocity * 0.65;

    var FrictionVector: rl.Vector3 = .{
        .x = rm.clamp(frictionFactor, 0, @abs(velocity.x)),
        .y = rm.clamp(frictionFactor, 0, @abs(velocity.y)),
        .z = rm.clamp(frictionFactor, 0, @abs(velocity.z)),
    };

    if (velocity.x > 0) FrictionVector.x = -FrictionVector.x;
    if (velocity.y > 0) FrictionVector.y = -FrictionVector.y;
    if (velocity.z > 0) FrictionVector.z = -FrictionVector.z;

    velocity.* = rm.vector3Add(velocity.*, FrictionVector);

    velocity.* = rm.vector3Clamp(velocity.*, rm.vector3Scale(velocityCeiling, -1), velocityCeiling);

    const tempPosition = rm.vector3Add(gameState.character.position, rm.vector3Scale(velocity.*, deltaTime));

    const previous = &gameState.character.faceDirection;
    if (previous.* != tempAngle) {
        if (gameState.character.model) |model| {
            rl.drawModelEx(model, gameState.character.position, heightAxis, previous.*, modelScale, rl.Color.white);
        }
        previous.* = tempAngle;
    } else {
        if (gameState.character.model) |model| {
            rl.drawModelEx(model, tempPosition, heightAxis, tempAngle, modelScale, rl.Color.white);
        }
        gameState.character.position = tempPosition;
    }

    gameState.camera.position = rm.vector3Add(rm.vector3Scale(cameraAngle, gameState.cameraDistance), gameState.character.position);
    rl.updateCamera(&gameState.camera, rl.CameraMode.camera_first_person);

    rl.drawGrid(20, 10.0);

    // Send the modifications back to source of truth
    // try gameState.node.send(messages.Payload{
    //     .update_character = .{
    //         .name = gameState.current_character.name,
    //         .x_position = gameState.current_character.x_position,
    //         .y_position = gameState.current_character.y_position,
    //         .map_name = gameState.current_character.map_name,
    //         .username = gameState.menu.login.username[0..gameState.menu.login.usernamePosition],
    //         .email = gameState.menu.email,
    //     },
    // });
    rl.endMode3D();
}
