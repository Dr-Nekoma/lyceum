const common = @import("common.zig");
const config = @import("../../config.zig");
const rl = @import("raylib");
const std = @import("std");
const GameState = @import("../../game/state.zig");

const slotBoxHeight = 40;

pub fn at(character: GameState.World.Character, width: f32) !void {
    const nameLength: f32 = @floatFromInt(rl.measureText(character.name, config.textFontSize));

    var index: [4]u8 = undefined;
    const levelNumberStr = std.fmt.bufPrintZ(index[0..], "{d}", .{character.level}) catch unreachable;
    const allocator: std.mem.Allocator = std.heap.c_allocator;
    const levelStr = try std.fmt.allocPrintZ(allocator, "{s}{s}", .{ "Lvl. ", levelNumberStr });
    const levelLength: f32 = @floatFromInt(rl.measureText(levelStr, config.textFontSize));

    const boundarySize: rl.Vector2 = .{
        .x = 12 * common.internalPadding + nameLength + levelLength,
        .y = slotBoxHeight,
    };
    const boundaryPosition: rl.Vector2 = .{
        .x = width / 2 - boundarySize.x / 2,
        .y = config.menuButtonsPadding,
    };

    rl.drawRectangleV(boundaryPosition, boundarySize, config.ColorPalette.primary);
    const iPositionX: i32 = @intFromFloat(boundaryPosition.x + 3 * common.internalPadding);
    const iPositionY: i32 = @intFromFloat(boundaryPosition.y + (boundarySize.y / 2 - config.textFontSize / 2));
    rl.drawText(character.name, iPositionX, iPositionY, config.textFontSize, rl.Color.white);
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
    rl.drawText(levelStr, levelPositionX, iPositionY, config.textFontSize, rl.Color.white);
}
