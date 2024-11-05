const config = @import("../../config.zig");
const rl = @import("raylib");

pub const Slot = struct {
    key: rl.KeyboardKey,
    label: [:0]const u8,
};

pub const slotBoxSize: rl.Vector2 = .{
    .x = 90,
    .y = 90,
};

pub const slotInternalSize: rl.Vector2 = .{
    .x = slotBoxSize.x * 4 / 5,
    .y = slotBoxSize.y * 4 / 5,
};

const textBoxSize: rl.Vector2 = .{
    .x = config.hubFontSize + 3,
    .y = config.hubFontSize + 4,
};

pub const internalPadding = 5;

fn drawBoundary(position: rl.Vector2) void {
    const thickness = 4;
    const slot = rl.Rectangle{
        .x = position.x,
        .y = position.y,
        .width = slotInternalSize.x + 2 * internalPadding,
        .height = slotInternalSize.y + 2 * internalPadding,
    };
    rl.drawRectangleLinesEx(slot, thickness, config.ColorPalette.secondary);
}

pub fn highlightSlots(position: rl.Vector2, keysAndLabels: []const Slot) void {
    var boundaryPosition: rl.Vector2 = .{
        .x = undefined,
        .y = position.y,
    };
    for (keysAndLabels, 0..) |keyAndLabel, pos| {
        const pos_float: f32 = @floatFromInt(pos);
        if (rl.isKeyDown(keyAndLabel.key)) {
            boundaryPosition.x = position.x + pos_float * slotInternalSize.x + pos_float * internalPadding;
            drawBoundary(boundaryPosition);
        }
    }
}

pub fn drawSlot(xPosition: f32, yPosition: f32, label: [:0]const u8, font: *rl.Font) void {
    rl.drawRectangleV(.{
        .x = xPosition,
        .y = yPosition,
    }, slotInternalSize, rl.Color.blue);
    rl.drawRectangleV(.{
        .x = xPosition,
        .y = yPosition,
    }, textBoxSize, rl.Color.black);
    rl.drawTextEx(font.*, label, .{ .x = xPosition, .y = yPosition }, config.hubFontSize, config.textSpacing, config.ColorPalette.secondary);
}
