const common = @import("common.zig");
const config = @import("../../config.zig");
const rl = @import("raylib");
const std = @import("std");
const GameState = @import("../../game/state.zig");

pub fn at(character: GameState.World.Character, width: f32, height: f32) !void {
    _ = character.faceDirection; // autofix
    const innerRadius = 130;
    const outerRadius = innerRadius + 10;

    const center: rl.Vector2 = .{
        .x = width - innerRadius - innerRadius / 2,
        .y = height - innerRadius - 20,
    };

    const triangle_top = center.add(.{ .y = -8, .x = 0 });
    const triangle_left = center.add(.{ .y = 4, .x = -6 });
    const triangle_right = center.add(.{ .y = 4, .x = 6 });

    std.debug.print("top: {}, left: {}, right: {}\n", .{
        triangle_top,
        triangle_left,
        triangle_right,
    });

    rl.drawCircleV(center, outerRadius, config.ColorPalette.primary);
    rl.drawCircleLinesV(center, innerRadius, rl.Color.white);
    rl.drawCircleLinesV(center, innerRadius - 1, rl.Color.white);
    rl.drawCircleV(center, innerRadius - 2, rl.Color.sky_blue);
    rl.drawTriangle(triangle_top, triangle_left, triangle_right, rl.Color.white);
}
