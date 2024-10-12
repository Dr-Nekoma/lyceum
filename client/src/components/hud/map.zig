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

    const map_x = center.x - outerRadius;
    const map_y = center.y - outerRadius;
    const map_mask = rl.Rectangle{
        // this x and y are the coordinates in the map image
        // TODO: dynamically compute x and y based on world coordinates
        .x = 10,
        .y = 10,
        .width = outerRadius * 2,
        .height = outerRadius * 2,
    };
    var map = rl.imageFromImage(character.inventory.hud.map.?, map_mask);
    defer map.unload();

    var alpha_mask = rl.genImageColor(
        @intFromFloat(map_mask.width),
        @intFromFloat(map_mask.height),
        rl.Color.black,
    );
    defer alpha_mask.unload();

    alpha_mask.drawCircle(
        outerRadius,
        outerRadius,
        innerRadius,
        rl.Color.white,
    );
    map.alphaMask(alpha_mask);

    const texture = map.toTexture();
    // FIXME: we are leaking the texture
    // unload() doesn't work because of timing issues
    // defer texture.unload();

    texture.draw(@intFromFloat(map_x), @intFromFloat(map_y), rl.Color.white);
    rl.drawRing(center, innerRadius, outerRadius, 0, 360, 0, config.ColorPalette.primary);
    rl.drawCircleLinesV(center, innerRadius, rl.Color.white);
    rl.drawCircleLinesV(center, innerRadius - 1, rl.Color.white);
    rl.drawTriangle(
        center.add(triangle_top),
        center.add(triangle_left),
        center.add(triangle_right),
        rl.Color.white,
    );
}
