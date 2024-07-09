const rl = @import("raylib");
const config = @import("../config.zig");
const button = @import("button.zig");
const std = @import("std");

ceiling: u32 = 999,
current: *u32,

text: [:0]const u8,
textPosition: rl.Vector2,
textSize: rl.Vector2,
textColor: rl.Color,

pub fn at(
    self: @This(),
) !void {
    const padding = 15;

    const messageSize: f32 = @floatFromInt(rl.measureText(self.text, config.buttonFontSize));
    const messageX = self.textPosition.x;
    const floatFont: f32 = @floatFromInt(config.buttonFontSize);
    const messageY = self.textPosition.y + self.textSize.y / 2 - floatFont / 2;
    rl.drawText(
        self.text,
        @intFromFloat(messageX),
        @intFromFloat(messageY),
        config.buttonFontSize,
        self.textColor,
    );

    const attrButtonSize: rl.Vector2 = .{
        .x = self.textSize.x,
        .y = self.textSize.y,
    };

    const minusPosition: rl.Vector2 = .{
        .x = self.textPosition.x + messageSize + padding,
        .y = messageY,
    };

    var buf: [3:0]u8 = .{ 0, 0, 0 };
    _ = try std.fmt.bufPrint(&buf, "{}", .{self.current.*});
    const currentMessageSize: f32 = @floatFromInt(rl.measureText(&buf, config.buttonFontSize));
    const currentMessageX = minusPosition.x + padding + attrButtonSize.x;
    rl.drawText(
        &buf,
        @intFromFloat(currentMessageX),
        @intFromFloat(messageY),
        config.buttonFontSize,
        self.textColor,
    );

    const plusPosition: rl.Vector2 = .{
        .x = currentMessageX + currentMessageSize + padding,
        .y = messageY,
    };

    if (button.at(
        "+",
        plusPosition,
        attrButtonSize,
        config.ColorPalette.primary,
    )) self.current.* +|= 1;

    if (button.at(
        "-",
        minusPosition,
        attrButtonSize,
        config.ColorPalette.primary,
    )) self.current.* -|= 1;
}
