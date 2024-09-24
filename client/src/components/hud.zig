const config = @import("../config.zig");
const rl = @import("raylib");
const std = @import("std");

const slotBoxSize: rl.Vector2 = .{
    .x = 90,
    .y = 90,
};

const slotInternalSize: rl.Vector2 = .{
    .x = slotBoxSize.x * 4 / 5,
    .y = slotBoxSize.y * 4 / 5,
};

const textBoxSize: rl.Vector2 = .{
    .x = config.hubFontSize + 4,
    .y = config.hubFontSize + 6,
};

const internalPadding = 5;

fn drawBoundary(position: rl.Vector2) void {
    var topLeftCorner: rl.Vector2 = .{
        .x = position.x,
        .y = position.y + 2.5,
    };
    var rightTopCorner: rl.Vector2 = .{
        .x = position.x + slotInternalSize.x + 1.5 * internalPadding,
        .y = position.y + 2.5,
    };
    const thickness = 4;
    rl.drawLineEx(topLeftCorner, rightTopCorner, thickness, rl.Color.white);
    var rightDownCorner: rl.Vector2 = .{
        .x = rightTopCorner.x,
        .y = rightTopCorner.y + slotInternalSize.y + 1.5 * internalPadding,
    };
    rightTopCorner.y -= 2.25;
    rl.drawLineEx(rightTopCorner, rightDownCorner, thickness, rl.Color.white);
    var leftDownCorner: rl.Vector2 = .{
        .x = rightDownCorner.x - slotInternalSize.x - 1.15 * internalPadding,
        .y = rightDownCorner.y - 2,
    };
    topLeftCorner.x = rightDownCorner.x - slotInternalSize.x - 1.15 * internalPadding;
    rightDownCorner.x += 2;
    rightDownCorner.y -= 2;
    rl.drawLineEx(rightDownCorner, leftDownCorner, thickness, rl.Color.white);
    leftDownCorner.y += 1.75;
    rl.drawLineEx(leftDownCorner, topLeftCorner, thickness, rl.Color.white);
}

fn highlightSlot(position: rl.Vector2, length: usize) void {
    var boundaryPosition: rl.Vector2 = .{
        .x = position.x,
        .y = position.y,
    };
    if (rl.isKeyDown(.key_one)) {
        if (length >= 1) {
            boundaryPosition.x += 0;
            drawBoundary(boundaryPosition);
        }
    } else if (rl.isKeyDown(.key_two)) {
        if (length >= 2) {
            boundaryPosition.x += slotInternalSize.x + internalPadding;
            drawBoundary(boundaryPosition);
        }
    } else if (rl.isKeyDown(.key_three)) {
        if (length >= 3) {
            boundaryPosition.x += 2 * slotInternalSize.x + 2 * internalPadding;
            drawBoundary(boundaryPosition);
        }
    } else if (rl.isKeyDown(.key_four)) {
        if (length >= 4) {
            boundaryPosition.x += 3 * slotInternalSize.x + 3 * internalPadding;
            drawBoundary(boundaryPosition);
        }
    } else if (rl.isKeyDown(.key_five)) {
        if (length >= 5) {
            boundaryPosition.x += 4 * slotInternalSize.x + 4 * internalPadding;
            drawBoundary(boundaryPosition);
        }
    } else if (rl.isKeyDown(.key_six)) {
        if (length >= 6) {
            boundaryPosition.x += 5 * slotInternalSize.x + 5 * internalPadding;
            drawBoundary(boundaryPosition);
        }
    } else if (rl.isKeyDown(.key_seven)) {
        if (length >= 7) {
            boundaryPosition.x += 6 * slotInternalSize.x + 6 * internalPadding;
            drawBoundary(boundaryPosition);
        }
    } else if (rl.isKeyDown(.key_eight)) {
        if (length >= 8) {
            boundaryPosition.x += 7 * slotInternalSize.x + 7 * internalPadding;
            drawBoundary(boundaryPosition);
        }
    } else if (rl.isKeyDown(.key_nine)) {
        if (length >= 9) {
            boundaryPosition.x += 8 * slotInternalSize.x + 8 * internalPadding;
            drawBoundary(boundaryPosition);
        }
    } else if (rl.isKeyDown(.key_zero)) {
        if (length >= 10) {
            boundaryPosition.x += 9 * slotInternalSize.x + 9 * internalPadding;
            drawBoundary(boundaryPosition);
        }
    }
}

pub fn at(
    // TODO: String file names are temporary for item slots
    slots: []const [:0]const u8,
    width: f32,
    height: f32,
) !void {
    const length: f32 = if (slots.len <= 10) @floatFromInt(slots.len) else 10.0;
    const boundarySize: rl.Vector2 = .{
        .x = slotInternalSize.x * length + (internalPadding * (length + 1)),
        .y = slotInternalSize.y + 2 * internalPadding,
    };
    const boundaryPosition: rl.Vector2 = .{
        .x = width / 2 - boundarySize.x / 2,
        .y = height - slotBoxSize.y - config.menuButtonsPadding,
    };

    rl.drawRectangleV(boundaryPosition, boundarySize, config.ColorPalette.primary);

    var index: [3]u8 = undefined;

    var xPosition = boundaryPosition.x + internalPadding;
    const yPosition = boundaryPosition.y + internalPadding;
    for (0.., slots[0..@intFromFloat(length)]) |i, elem| {
        std.debug.print("{}: {s}\n", .{ i, elem });
        rl.drawRectangleV(.{
            .x = xPosition,
            .y = yPosition,
        }, slotInternalSize, rl.Color.blue);
        rl.drawRectangleV(.{
            .x = xPosition,
            .y = yPosition,
        }, textBoxSize, rl.Color.black);
        const strIndex = std.fmt.bufPrintZ(index[0..], "{d}", .{i + 1}) catch unreachable;
        rl.drawText(strIndex, @intFromFloat(xPosition), @intFromFloat(yPosition), config.hubFontSize, rl.Color.white);
        xPosition += internalPadding + slotInternalSize.x;
    }
    highlightSlot(boundaryPosition, slots.len);
}
