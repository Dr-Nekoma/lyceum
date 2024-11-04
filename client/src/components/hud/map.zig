const common = @import("common.zig");
const config = @import("../../config.zig");
const rl = @import("raylib");
const rm = rl.math;
const std = @import("std");
const GameState = @import("../../game/state.zig");

pub fn init_map_texture() rl.Texture {
    const side = config.map.border_thickness * 2;
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
    fn worldToMap(position: rl.Vector2, world: *const GameState.World.Map) struct { f32, f32 } {
        const character_x: f32 = rm.clamp(position.x, 0, @floatFromInt(world.instance.width * config.map.mini_map_size));
        const character_y: f32 = rm.clamp(position.y, 0, @floatFromInt(world.instance.height * config.map.mini_map_size));
        return .{ character_x, character_y };
    }
    pub fn normalize(position: rl.Vector2, map_image: *const rl.Image, world: *const GameState.World.Map) struct { f32, f32 } {
        const character_x, const character_y = worldToMap(position, world);
        const fWidth: f32 = @floatFromInt(world.instance.width);
        const fHeight: f32 = @floatFromInt(world.instance.height);
        const normalized_x = character_x * @as(f32, @as(f32, @floatFromInt(map_image.width - 2 * config.map.border_thickness)) / (config.assets.tile.size * fWidth));
        const normalized_y = character_y * @as(f32, @as(f32, @floatFromInt(map_image.height - 2 * config.map.border_thickness)) / (config.assets.tile.size * fHeight));
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
    const normalized_x, const normalized_y = coordinates.normalize(position, &character.inventory.hud.minimap.map.?, world);

    const center: rl.Vector2 = getCenter(width, height);
    const map_image = character.inventory.hud.minimap.map.?;
    const map_x = center.x - outerRadius;
    const map_y = center.y - outerRadius;
    const map_mask = rl.Rectangle{
        .x = normalized_x,
        .y = normalized_y,
        .width = @floatFromInt(@min(outerRadius * 2, map_image.width)),
        .height = @floatFromInt(@min(outerRadius * 2, map_image.height)),
    };
    var map = rl.imageFromImage(map_image, map_mask);
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
        config.ColorPalette.secondary,
    );
    map.alphaMask(alpha_mask);

    const pixels = try rl.loadImageColors(map);
    const texture = character.inventory.hud.minimap.texture.?;
    rl.updateTexture(texture, pixels.ptr);

    texture.draw(@intFromFloat(map_x), @intFromFloat(map_y), config.ColorPalette.secondary);
    rl.drawRing(center, innerRadius, outerRadius, 0, 360, 0, config.ColorPalette.primary);
    rl.drawCircleLinesV(center, innerRadius, config.ColorPalette.secondary);
    rl.drawCircleLinesV(center, innerRadius - 1, config.ColorPalette.secondary);

    player(character.stats.face_direction, center);

    drawMapName(center, character.stats.map_name, font);
}
