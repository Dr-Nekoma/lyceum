const rl = @import("raylib");
const rm = rl.math;
const std = @import("std");
const GameState = @import("../game/state.zig");

pub const heightAxis: rl.Vector3 = .{
    .x = 0,
    .y = 1,
    .z = 0,
};

pub const character = struct {
    pub const modelScale: rl.Vector3 = .{
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
    pub const floorLevel = 12;
    const ceilingLevel = 48;

    pub fn draw(gameState: *GameState) void {
        var tempAngle = gameState.world.character.stats.face_direction;
        const velocity = &gameState.world.character.velocity;
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

        var tempPosition: rl.Vector3 = rm.vector3Add(gameState.world.character.position, rm.vector3Scale(velocity.*, deltaTime));
        tempPosition.y = rm.clamp(tempPosition.y, floorLevel, ceilingLevel);

        const previous = &gameState.world.character.stats.face_direction;
        if (previous.* != tempAngle) {
            if (gameState.world.character.model) |model| {
                rl.drawModelEx(model, gameState.world.character.position, heightAxis, @floatFromInt(previous.*), modelScale, rl.Color.white);
            }
            previous.* = tempAngle;
        } else {
            if (gameState.world.character.model) |model| {
                rl.drawModelEx(model, tempPosition, heightAxis, @floatFromInt(tempAngle), modelScale, rl.Color.white);
            }
            gameState.world.character.position = tempPosition;
            gameState.world.character.stats.x_position = @intFromFloat(tempPosition.x);
            gameState.world.character.stats.y_position = @intFromFloat(tempPosition.z);
        }
    }
};
