const common = @import("common.zig");
const config = @import("../../config.zig");
const rl = @import("raylib");

const availableKeysAndLabels = [_]common.Slot{
    .{ .key = .key_q, .label = "Q" },
    .{ .key = .key_e, .label = "E" },
};

pub fn at(slots: []const [:0]const u8, height: f32, font: *rl.Font) !void {
    const length: usize = if (slots.len <= 2) slots.len else 2;
    const keysAndLabels = availableKeysAndLabels[0..length];

    const fLength: f32 = @floatFromInt(length);
    const boundarySize: rl.Vector2 = .{
        .x = common.slotInternalSize.x * fLength + (common.internalPadding * (fLength + 1)),
        .y = common.slotInternalSize.y + 2 * common.internalPadding,
    };
    const boundaryPosition: rl.Vector2 = .{
        .x = 250,
        .y = height - common.slotBoxSize.y - 6,
    };

    rl.drawRectangleV(boundaryPosition, boundarySize, config.ColorPalette.primary);

    var xPosition = boundaryPosition.x + common.internalPadding;
    const yPosition = boundaryPosition.y + common.internalPadding;

    for (keysAndLabels) |keyAndLabel| {
        common.drawSlot(xPosition, yPosition, keyAndLabel.label, font);
        xPosition += common.internalPadding + common.slotInternalSize.x;
    }

    common.highlightSlots(boundaryPosition, keysAndLabels);
}
