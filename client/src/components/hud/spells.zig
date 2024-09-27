const common = @import("common.zig");
const config = @import("../../config.zig");
const rl = @import("raylib");
const std = @import("std");

fn highlightSpellSlots(position: rl.Vector2, length: usize) void {
    var boundaryPosition: rl.Vector2 = .{
        .x = position.x,
        .y = position.y,
    };
    if (rl.isKeyDown(.key_one)) {
        if (length >= 1) {
            boundaryPosition.x += 0;
            common.drawBoundary(boundaryPosition);
        }
    } else if (rl.isKeyDown(.key_two)) {
        if (length >= 2) {
            boundaryPosition.x += common.slotInternalSize.x + common.internalPadding;
            common.drawBoundary(boundaryPosition);
        }
    } else if (rl.isKeyDown(.key_three)) {
        if (length >= 3) {
            boundaryPosition.x += 2 * common.slotInternalSize.x + 2 * common.internalPadding;
            common.drawBoundary(boundaryPosition);
        }
    } else if (rl.isKeyDown(.key_four)) {
        if (length >= 4) {
            boundaryPosition.x += 3 * common.slotInternalSize.x + 3 * common.internalPadding;
            common.drawBoundary(boundaryPosition);
        }
    } else if (rl.isKeyDown(.key_five)) {
        if (length >= 5) {
            boundaryPosition.x += 4 * common.slotInternalSize.x + 4 * common.internalPadding;
            common.drawBoundary(boundaryPosition);
        }
    } else if (rl.isKeyDown(.key_six)) {
        if (length >= 6) {
            boundaryPosition.x += 5 * common.slotInternalSize.x + 5 * common.internalPadding;
            common.drawBoundary(boundaryPosition);
        }
    } else if (rl.isKeyDown(.key_seven)) {
        if (length >= 7) {
            boundaryPosition.x += 6 * common.slotInternalSize.x + 6 * common.internalPadding;
            common.drawBoundary(boundaryPosition);
        }
    } else if (rl.isKeyDown(.key_eight)) {
        if (length >= 8) {
            boundaryPosition.x += 7 * common.slotInternalSize.x + 7 * common.internalPadding;
            common.drawBoundary(boundaryPosition);
        }
    } else if (rl.isKeyDown(.key_nine)) {
        if (length >= 9) {
            boundaryPosition.x += 8 * common.slotInternalSize.x + 8 * common.internalPadding;
            common.drawBoundary(boundaryPosition);
        }
    } else if (rl.isKeyDown(.key_zero)) {
        if (length >= 10) {
            boundaryPosition.x += 9 * common.slotInternalSize.x + 9 * common.internalPadding;
            common.drawBoundary(boundaryPosition);
        }
    }
}

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
    for (0.., slots[0..@intFromFloat(length)]) |i, elem| {
        std.debug.print("{}: {s}\n", .{ i, elem });
        const strIndex = std.fmt.bufPrintZ(index[0..], "{d}", .{i + 1}) catch unreachable;
        common.drawSlot(xPosition, yPosition, strIndex);
        xPosition += common.internalPadding + common.slotInternalSize.x;
    }
    highlightSpellSlots(boundaryPosition, slots.len);
}
