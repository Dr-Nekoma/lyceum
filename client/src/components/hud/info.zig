const common = @import("common.zig");
const config = @import("../../config.zig");
const rl = @import("raylib");
const std = @import("std");
const GameState = @import("../../game/state.zig");

const barHeight = 40;

fn stats(character: GameState.World.Character, size: rl.Vector2, position: rl.Vector2, fontSize: i32, allocator: std.mem.Allocator) !void {
    const nameLength: f32 = @floatFromInt(rl.measureText(character.stats.name, fontSize));

    var index: [4]u8 = undefined;
    const levelNumberStr = std.fmt.bufPrintZ(index[0..], "{d}", .{character.stats.level}) catch unreachable;
    const levelStr = try std.fmt.allocPrintZ(allocator, "{s}{s}", .{ "Lvl. ", levelNumberStr });
    const levelLength: f32 = @floatFromInt(rl.measureText(levelStr, fontSize));

    const boundarySize: rl.Vector2 = .{
        .x = size.x + nameLength + levelLength,
        .y = size.y,
    };

    const boundaryPosition: rl.Vector2 = .{
        .x = position.x - boundarySize.x / 2,
        .y = position.y,
    };

    rl.drawRectangleV(boundaryPosition, boundarySize, config.ColorPalette.primary);
    const iPositionX: i32 = @intFromFloat(boundaryPosition.x + 3 * common.internalPadding);
    const floatFontSize: f32 = @floatFromInt(fontSize);
    const iPositionY: i32 = @intFromFloat(boundaryPosition.y + (boundarySize.y / 2 - floatFontSize / 2));
    rl.drawText(character.stats.name, iPositionX, iPositionY, fontSize, rl.Color.white);
    const top: rl.Vector2 = .{
        .x = boundaryPosition.x + nameLength + 6 * common.internalPadding,
        .y = boundaryPosition.y,
    };
    const down: rl.Vector2 = .{
        .x = top.x,
        .y = boundaryPosition.y + boundarySize.y,
    };
    const thickness = 4;
    rl.drawLineEx(top, down, thickness, rl.Color.white);
    const levelPositionX: i32 = @intFromFloat(top.x + 3 * common.internalPadding);
    rl.drawText(levelStr, levelPositionX, iPositionY, fontSize, rl.Color.white);
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

fn faceStats(character: GameState.World.Character) !void {
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
    rl.drawText(healthNumberStr, healthBarPosition.x + 15, healthNumberPositionY, config.textFontSize, rl.Color.black);

    const manaNumberStr = std.fmt.bufPrintZ(index[0..], "{d}", .{character.stats.mana}) catch unreachable;
    const manaNumberPositionY: i32 = @intFromFloat(manaBarPosition.y + (0.75 * barHeight / 2 - config.textFontSize / 2));

    bar(character.stats.mana, character.stats.mana_max, manaBarPosition, 300, rl.Color.blue, rl.Color.dark_blue);
    rl.drawText(manaNumberStr, manaBarPosition.x + 15, manaNumberPositionY, config.textFontSize, rl.Color.black);

    rl.drawCircleV(center, outerRadius, config.ColorPalette.primary);
    rl.drawCircleLinesV(center, innerRadius, rl.Color.white);
    rl.drawCircleLinesV(center, innerRadius - 1, rl.Color.white);
    rl.drawCircleV(center, innerRadius - 2, rl.Color.sky_blue);
}

pub const mainSize: rl.Vector2 = .{
    .x = 12 * common.internalPadding,
    .y = barHeight,
};

pub fn at(character: GameState.World.Character, size: rl.Vector2, position: rl.Vector2, fontSize: i32, allocator: std.mem.Allocator) !void {
    try stats(character, size, position, fontSize, allocator);
    try faceStats(character);
}
