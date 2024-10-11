const common = @import("common.zig");
const config = @import("../../config.zig");
const rl = @import("raylib");
const std = @import("std");
const GameState = @import("../../game/state.zig");

fn rotate_point(
    position: rl.Vector2,
    origin: rl.Vector2,
    degrees: f32,
) rl.Vector2 {
    const radians = degrees * (std.math.pi / 180.0);
    const dx = position.x - origin.x;
    const dy = position.y - origin.y;
    return .{
        .x = origin.x + dx * @cos(radians) - dy * @sin(radians),
        .y = origin.y + dx * @sin(radians) + dy * @cos(radians),
    };
}

pub fn at(character: GameState.World.Character, width: f32, height: f32) !void {
    const face_direction = character.faceDirection;
    const innerRadius = 130;
    const outerRadius = innerRadius + 10;

    const origin = .{ .x = 0, .y = 0 };
    const triangle_top = rotate_point(.{ .y = -8, .x = 0 }, origin, face_direction);
    const triangle_left = rotate_point(.{ .y = 4, .x = -4 }, origin, face_direction);
    const triangle_right = rotate_point(.{ .y = 4, .x = 4 }, origin, face_direction);

    const center: rl.Vector2 = .{
        .x = width - innerRadius - innerRadius / 2,
        .y = height - innerRadius - 20,
    };

    std.debug.print("top: {}, left: {}, right: {}\n", .{
        triangle_top,
        triangle_left,
        triangle_right,
    });

    rl.drawCircleV(center, outerRadius, config.ColorPalette.primary);
    rl.drawCircleLinesV(center, innerRadius, rl.Color.white);
    rl.drawCircleLinesV(center, innerRadius - 1, rl.Color.white);
    rl.drawCircleV(center, innerRadius - 2, rl.Color.sky_blue);
    rl.drawTriangle(
        center.add(triangle_top),
        center.add(triangle_left),
        center.add(triangle_right),
        rl.Color.white,
    );
}
