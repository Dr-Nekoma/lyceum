const rl = @import("raylib");
const rm = rl.math;
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

    pub fn draw(entity: *GameState.World.Character, tempAngle: u16) void {
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
            entity.position = tempPosition;
            entity.stats.x_position = tempPosition.x;
            entity.stats.y_position = tempPosition.z;
            entity.stats.x_velocity = velocity.x;
            entity.stats.y_velocity = velocity.z;
        }
    }
};
