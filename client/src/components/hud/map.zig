const common = @import("common.zig");
const config = @import("../../config.zig");
const rl = @import("raylib");
const rm = rl.math;
const std = @import("std");
const GameState = @import("../../game/state.zig");

pub fn init_map_texture() rl.Texture {
    const side = outerRadius * 2;
    const img = rl.genImageColor(side, side, rl.Color.black);
    defer img.unload();
    return img.toTexture();
}

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

pub fn add_borders(image: *rl.Image) void {
    const thickness = config.map.border_thickness;
    const width = image.width + 2 * thickness;
    const height = image.height + 2 * thickness;
    return image.resizeCanvas(width, height, thickness, thickness, rl.Color.black);
}

pub fn player(world_face_direction: i16, center: rl.Vector2) void {
    const face_direction: f32 = @floatFromInt(@mod(180 - world_face_direction, 360));
    const origin = .{ .x = 0, .y = 0 };
    const triangle_top = rotate_point(.{ .y = -8, .x = 0 }, origin, face_direction);
    const triangle_left = rotate_point(.{ .y = 4, .x = -4 }, origin, face_direction);
    const triangle_right = rotate_point(.{ .y = 4, .x = 4 }, origin, face_direction);
    rl.drawTriangle(
        center.add(triangle_top),
        center.add(triangle_left),
        center.add(triangle_right),
        config.ColorPalette.secondary,
    );
}

const outerRadius = config.map.border_thickness;
pub const innerRadius = outerRadius - 10;

pub fn getCenter(width: f32, height: f32) rl.Vector2 {
    return .{
        .x = width - innerRadius - innerRadius / 2,
        .y = height - innerRadius - 20,
    };
}

pub const coordinates = struct {
    pub fn normalize(position: rl.Vector2, map_image: *const rl.Image, world: *const GameState.World.Map) struct { f32, f32 } {
        const character_x = position.x;
        const character_y = position.y;
        const fWidth: f32 = @floatFromInt(world.instance.width);
        const fHeight: f32 = @floatFromInt(world.instance.height);
        const normalized_x = character_x * @as(f32, @as(f32, @floatFromInt(map_image.width)) / (config.assets.tile.size * fWidth));
        const normalized_y = character_y * @as(f32, @as(f32, @floatFromInt(map_image.height)) / (config.assets.tile.size * fHeight));
        return .{ normalized_x, normalized_y };
    }
};

fn drawMapName(center: rl.Vector2, name: [:0]const u8, font: *rl.Font) void {
    const nameMeasure = rl.measureTextEx(font.*, name, config.textFontSize, config.textSpacing);
    const position: rl.Vector2 = .{
        .x = center.x - nameMeasure.x / 2,
        .y = center.y - innerRadius - (nameMeasure.y / 2),
    };

    const border = 10;
    const bannerSize: rl.Vector2 = .{
        .x = nameMeasure.x + 2 * border,
        .y = config.textFontSize + 2 * border,
    };
    const bannerPosition: rl.Vector2 = .{
        .x = position.x - border,
        .y = position.y - border,
    };

    rl.drawRectangleV(bannerPosition, bannerSize, config.ColorPalette.primary);
    rl.drawRectangleLinesEx(.{ .x = bannerPosition.x, .y = bannerPosition.y, .width = bannerSize.x, .height = bannerSize.y }, 2, config.ColorPalette.secondary);

    rl.drawTextEx(
        font.*,
        name,
        position,
        config.textFontSize,
        config.textSpacing,
        config.ColorPalette.secondary,
    );
}

pub fn at(character: *const GameState.World.Character, world: *const GameState.World.Map, width: f32, height: f32, font: *rl.Font) !void {
    const position: rl.Vector2 = .{
        .x = character.stats.x_position,
        .y = character.stats.y_position,
    };
    const map_image = character.inventory.hud.minimap.map.?;

    const normalized_x, const normalized_y = coordinates.normalize(position, &map_image, world);
    const displacement_x: f32 = normalized_x - @as(f32, @floatFromInt(@divFloor(map_image.width, 2)));
    const displacement_y: f32 = normalized_y - @as(f32, @floatFromInt(@divFloor(map_image.height, 2)));

    const center: rl.Vector2 = getCenter(width, height);

    var canvas: rl.Image = rl.genImageColor(@intFromFloat(2 * outerRadius), @intFromFloat(2 * outerRadius), rl.Color.black);
    defer canvas.unload();

    const map_mask = rl.Rectangle{
        .x = @max(0, displacement_x),
        .y = @max(0, displacement_y),
        .width = @floatFromInt(@min(outerRadius * 2, map_image.width)),
        .height = @floatFromInt(@min(outerRadius * 2, map_image.height)),
    };

    const target: rl.Rectangle = .{
        .x = -displacement_x,
        .y = -displacement_y,
        .width = map_mask.width,
        .height = map_mask.height,
    };

    canvas.drawImage(map_image, map_mask, target, rl.Color.white);

    var alpha_mask = rl.genImageColor(
        @intFromFloat(outerRadius * 2),
        @intFromFloat(outerRadius * 2),
        rl.Color.black,
    );
    defer alpha_mask.unload();

    alpha_mask.drawCircle(
        outerRadius,
        outerRadius,
        innerRadius,
        config.ColorPalette.secondary,
    );

    canvas.alphaMask(alpha_mask);

    const pixels = try rl.loadImageColors(canvas);

    const texture = character.inventory.hud.minimap.texture.?;
    rl.updateTexture(texture, pixels.ptr);

    const map_x = center.x - outerRadius;
    const map_y = center.y - outerRadius;
    texture.draw(@intFromFloat(map_x), @intFromFloat(map_y), rl.Color.white);
    rl.drawRing(center, innerRadius, outerRadius, 0, 360, 0, config.ColorPalette.primary);
    rl.drawCircleLinesV(center, innerRadius, config.ColorPalette.secondary);
    rl.drawCircleLinesV(center, innerRadius - 1, config.ColorPalette.secondary);
    player(character.stats.face_direction, center);

    drawMapName(center, character.stats.map_name, font);
}
