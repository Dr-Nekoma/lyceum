const common = @import("common.zig");
const config = @import("../../config.zig");
const rl = @import("raylib");

const consumableKeys = .{ "Q", "E" };

fn hightlightSlots(position: rl.Vector2) void {
    var boundaryPosition: rl.Vector2 = .{
        .x = position.x,
        .y = position.y,
    };
    if (rl.isKeyDown(.key_q)) {
        boundaryPosition.x += 0;
        common.drawBoundary(boundaryPosition);
    } else if (rl.isKeyDown(.key_e)) {
        boundaryPosition.x += common.slotInternalSize.x + common.internalPadding;
        common.drawBoundary(boundaryPosition);
    }
}

pub fn at(slots: []const [:0]const u8, height: f32) !void {
    const length: f32 = if (slots.len <= 2) @floatFromInt(slots.len) else 2.0;
    const boundarySize: rl.Vector2 = .{
        .x = common.slotInternalSize.x * length + (common.internalPadding * (length + 1)),
        .y = common.slotInternalSize.y + 2 * common.internalPadding,
    };
    const boundaryPosition: rl.Vector2 = .{
        .x = 250,
        .y = height - common.slotBoxSize.y - config.menuButtonsPadding,
    };

    rl.drawRectangleV(boundaryPosition, boundarySize, config.ColorPalette.primary);

    var xPosition = boundaryPosition.x + common.internalPadding;
    const yPosition = boundaryPosition.y + common.internalPadding;

    common.drawSlot(xPosition, yPosition, consumableKeys[0]);
    xPosition += common.internalPadding + common.slotInternalSize.x;
    common.drawSlot(xPosition, yPosition, consumableKeys[1]);
    const keys = [_]rl.KeyboardKey{
        .key_q,
        .key_e,
    };
    if (length < keys.len) {
        return error.too_many_keys;
    }
    common.highlightSlots(boundaryPosition, &keys);
}
