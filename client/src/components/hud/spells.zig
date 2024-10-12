const common = @import("common.zig");
const config = @import("../../config.zig");
const rl = @import("raylib");
const std = @import("std");

pub fn at(slots: []const [:0]const u8, width: f32, height: f32) !void {
    const length: f32 = if (slots.len <= 10) @floatFromInt(slots.len) else 10.0;
    const boundarySize: rl.Vector2 = .{
        .x = common.slotInternalSize.x * length + (common.internalPadding * (length + 1)),
        .y = common.slotInternalSize.y + 2 * common.internalPadding,
    };
    const boundaryPosition: rl.Vector2 = .{
        .x = width / 2 - boundarySize.x / 2,
        .y = height - common.slotBoxSize.y - config.menuButtonsPadding,
    };

    rl.drawRectangleV(boundaryPosition, boundarySize, config.ColorPalette.primary);

    var index: [3]u8 = undefined;

    var xPosition = boundaryPosition.x + common.internalPadding;
    const yPosition = boundaryPosition.y + common.internalPadding;
    for (0.., slots[0..@intFromFloat(length)]) |i, _| {
        const strIndex = std.fmt.bufPrintZ(index[0..], "{d}", .{i + 1}) catch unreachable;
        common.drawSlot(xPosition, yPosition, strIndex);
        xPosition += common.internalPadding + common.slotInternalSize.x;
    }
    const keys = [_]rl.KeyboardKey{
        .key_one,
        .key_two,
        .key_three,
        .key_four,
        .key_five,
        .key_six,
        .key_seven,
        .key_eight,
        .key_nine,
        .key_zero,
    };
    if (length < keys.len) {
        return error.too_many_keys;
    }
    common.highlightSlots(boundaryPosition, &keys);
}
