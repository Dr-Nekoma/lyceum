const common = @import("common.zig");
const config = @import("../../config.zig");
const rl = @import("raylib");
const std = @import("std");

const availableKeysAndLabels = [_]common.Slot{
    .{ .key = .key_one, .label = "1" },
    .{ .key = .key_two, .label = "2" },
    .{ .key = .key_three, .label = "3" },
    .{ .key = .key_four, .label = "4" },
    .{ .key = .key_five, .label = "5" },
    .{ .key = .key_six, .label = "6" },
    .{ .key = .key_seven, .label = "7" },
    .{ .key = .key_eight, .label = "8" },
    .{ .key = .key_nine, .label = "9" },
    .{ .key = .key_zero, .label = "0" },
};

pub fn at(slots: []const [:0]const u8, width: f32, height: f32) !void {
    const length: usize = if (slots.len <= 10) slots.len else 10;
    const keysAndLabels = availableKeysAndLabels[0..length];

    const fLength: f32 = @floatFromInt(length);

    const boundarySize: rl.Vector2 = .{
        .x = common.slotInternalSize.x * fLength + (common.internalPadding * (fLength + 1)),
        .y = common.slotInternalSize.y + 2 * common.internalPadding,
    };
    const boundaryPosition: rl.Vector2 = .{
        .x = width / 2 - boundarySize.x / 2,
        .y = height - common.slotBoxSize.y - config.menuButtonsPadding,
    };

    rl.drawRectangleV(boundaryPosition, boundarySize, config.ColorPalette.primary);

    var xPosition = boundaryPosition.x + common.internalPadding;
    const yPosition = boundaryPosition.y + common.internalPadding;
    for (keysAndLabels) |keyAndLabel| {
        common.drawSlot(xPosition, yPosition, keyAndLabel.label);
        xPosition += common.internalPadding + common.slotInternalSize.x;
    }
    common.highlightSlots(boundaryPosition, keysAndLabels);
}
