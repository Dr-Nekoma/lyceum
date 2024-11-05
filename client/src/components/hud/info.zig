const common = @import("common.zig");
const config = @import("../../config.zig");
const rl = @import("raylib");
const std = @import("std");
const GameState = @import("../../game/state.zig");

const barHeight = 40;

pub fn stats(character: *const GameState.World.Character, size: rl.Vector2, position: rl.Vector2, fontSize: f32, allocator: std.mem.Allocator, font: *rl.Font) !void {
    const nameLength: f32 = rl.measureTextEx(font.*, character.stats.name, fontSize, config.textSpacing).x;

    var index: [4]u8 = undefined;
    const levelNumberStr = try std.fmt.bufPrintZ(index[0..], "{d}", .{character.stats.level});
    const levelStr = try std.fmt.allocPrintZ(allocator, "{s}{s}", .{ "Lvl. ", levelNumberStr });
    defer allocator.free(levelStr);
    const levelLength: f32 = rl.measureTextEx(font.*, levelStr, fontSize, config.textSpacing).x;

    const boundarySize: rl.Vector2 = .{
        .x = size.x + nameLength + levelLength,
        .y = size.y,
    };

    const boundaryPosition: rl.Vector2 = .{
        .x = position.x - boundarySize.x / 2,
        .y = position.y,
    };

    rl.drawRectangleV(boundaryPosition, boundarySize, config.ColorPalette.primary);
    rl.drawRectangleLinesEx(.{ .x = boundaryPosition.x, .y = boundaryPosition.y, .width = boundarySize.x, .height = boundarySize.y }, 2, config.ColorPalette.secondary);
    const positionX: f32 = boundaryPosition.x + 3 * common.internalPadding;
    const positionY: f32 = boundaryPosition.y + (boundarySize.y / 2 - fontSize / 2);
    rl.drawTextEx(font.*, character.stats.name, .{ .x = positionX, .y = positionY }, fontSize, config.textSpacing, config.ColorPalette.secondary);
    const top: rl.Vector2 = .{
        .x = boundaryPosition.x + nameLength + 6 * common.internalPadding,
        .y = boundaryPosition.y,
    };
    const down: rl.Vector2 = .{
        .x = top.x,
        .y = boundaryPosition.y + boundarySize.y,
    };
    const thickness = 4;
    rl.drawLineEx(top, down, thickness, config.ColorPalette.secondary);
    const levelPositionX: f32 = top.x + 3 * common.internalPadding;
    rl.drawTextEx(font.*, levelStr, .{ .x = levelPositionX, .y = positionY }, fontSize, config.textSpacing, config.ColorPalette.secondary);
}

fn bar(currentValue: u32, maxValue: u32, position: rl.Vector2, barWidth: u32, inner_color: rl.Color, outer_color: rl.Color) void {
    const currentWidth: f32 = @floatFromInt((barWidth * currentValue) / maxValue);
    rl.drawRectangleV(position, .{ .x = currentWidth, .y = barHeight * 0.75 }, inner_color);
    const fBarWidth: f32 = @floatFromInt(barWidth);
    const rec: rl.Rectangle = .{
        .x = position.x,
        .y = position.y,
        .width = fBarWidth,
        .height = barHeight * 0.75,
    };
    rl.drawRectangleLinesEx(rec, 2, outer_color);
}

fn faceStats(character: *const GameState.World.Character, font: *rl.Font) !void {
    const innerRadius = 65;
    const outerRadius = innerRadius + 10;

    const center: rl.Vector2 = .{
        .x = 85,
        .y = 85,
    };

    const healthBarPosition: rl.Vector2 = .{
        .x = center.x + outerRadius - 6,
        .y = center.y - barHeight * 0.75,
    };

    const manaBarPosition: rl.Vector2 = .{
        .x = center.x + outerRadius - 6,
        .y = center.y,
    };

    var index: [4]u8 = undefined;
    const healthNumberStr = std.fmt.bufPrintZ(index[0..], "{d}", .{character.stats.health}) catch unreachable;
    const healthNumberPositionY: i32 = @intFromFloat(healthBarPosition.y + (0.75 * barHeight / 2 - config.textFontSize / 2));

    bar(character.stats.health, character.stats.health_max, healthBarPosition, 400, rl.Color.lime, rl.Color.green);
    rl.drawTextEx(font.*, healthNumberStr, .{ .x = healthBarPosition.x + 15, .y = healthNumberPositionY }, config.textFontSize, config.textSpacing, rl.Color.black);

    const manaNumberStr = std.fmt.bufPrintZ(index[0..], "{d}", .{character.stats.mana}) catch unreachable;
    const manaNumberPositionY: i32 = @intFromFloat(manaBarPosition.y + (0.75 * barHeight / 2 - config.textFontSize / 2));

    bar(character.stats.mana, character.stats.mana_max, manaBarPosition, 300, rl.Color.blue, rl.Color.dark_blue);
    rl.drawTextEx(font.*, manaNumberStr, .{ .x = manaBarPosition.x + 15, .y = manaNumberPositionY }, config.textFontSize, config.textSpacing, rl.Color.black);

    rl.drawCircleV(center, outerRadius, config.ColorPalette.primary);
    rl.drawCircleLinesV(center, innerRadius, config.ColorPalette.secondary);
    rl.drawCircleLinesV(center, innerRadius - 1, config.ColorPalette.secondary);
    rl.drawCircleV(center, innerRadius - 2, rl.Color.sky_blue);
}

pub const mainSize: rl.Vector2 = .{
    .x = 12 * common.internalPadding,
    .y = barHeight,
};

pub fn at(character: *const GameState.World.Character, size: rl.Vector2, position: rl.Vector2, fontSize: f32, allocator: std.mem.Allocator, font: *rl.Font) !void {
    try stats(character, size, position, fontSize, allocator, font);
    try faceStats(character, font);
}
