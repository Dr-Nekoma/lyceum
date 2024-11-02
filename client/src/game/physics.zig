const config = @import("../config.zig");
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
        .x = 18,
        .y = 18,
        .z = 18,
    };

    const velocityCeiling: rl.Vector3 = .{
        .x = 120,
        .y = 120,
        .z = 120,
    };

    pub const acceleration = 120;
    pub const floorLevel = 12;
    const ceilingLevel = 48;

    fn canMove(position: rl.Vector3, map: *const GameState.World.Map) bool {
        const width = map.instance.width;
        const height = map.instance.height;

        const x_tile: i32 = @intFromFloat(position.x / config.assets.tile.size);
        const y_tile: i32 = @intFromFloat(position.z / config.assets.tile.size);
        const iWidth: i32 = @intCast(width);
        const canWalk = map.instance.tiles[@intCast(iWidth * x_tile + y_tile)] != .water;

        const fWidth: f32 = @floatFromInt(width);
        const fHeight: f32 = @floatFromInt(height);
        const outsideLimits = (position.z > (fWidth + 1) * config.assets.tile.size or
            position.z < 0 or
            position.x > (fHeight + 1) * config.assets.tile.size or
            position.x < 0);
        std.debug.print("canWalk: {}, outsideLimits: {}\n", .{ canWalk, outsideLimits });

        return canWalk and !outsideLimits;
    }

    pub fn draw(entity: *GameState.World.Character, map: *const GameState.World.Map, tempAngle: u16) void {
        const velocity = &entity.velocity;
        const state = &entity.stats.state_type;
        const deltaTime = rl.getFrameTime();
        const deltaVelocity = deltaTime * acceleration;

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

        state.* = if (rm.vector3Equals(velocity.*, rm.vector3Zero()) == 0) .walking else .idle;

        var tempPosition: rl.Vector3 = rm.vector3Add(entity.position, rm.vector3Scale(velocity.*, deltaTime));
        tempPosition.y = rm.clamp(tempPosition.y, floorLevel, ceilingLevel);

        const previous = &entity.stats.face_direction;
        if (previous.* != tempAngle) {
            if (entity.model) |model| {
                rl.drawModelEx(model, entity.position, heightAxis, @floatFromInt(previous.*), modelScale, rl.Color.blue);
            }
            previous.* = tempAngle;
        } else {
            if (entity.model) |model| {
                rl.drawModelEx(model, tempPosition, heightAxis, @floatFromInt(tempAngle), modelScale, rl.Color.blue);
            }

            if (!canMove(tempPosition, map)) {
                velocity.* = rm.vector3Zero();
                return;
            }
            entity.position = tempPosition;
            entity.stats.x_position = tempPosition.x;
            entity.stats.y_position = tempPosition.z;
            entity.stats.x_velocity = velocity.x;
            entity.stats.y_velocity = velocity.z;
        }
    }
};
