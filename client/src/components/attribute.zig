const rl = @import("raylib");
const config = @import("../config.zig");
const std = @import("std");
const Button = @import("button.zig");

// TODO: Move this to another file to have proper generic types for each field
ceiling: u16 = 999,
current: *u8,

text: [:0]const u8,
textPosition: rl.Vector2,
textSize: rl.Vector2,
textColor: rl.Color,
sound: *rl.Sound,
font: *rl.Font,

pub fn at(
    self: @This(),
) !void {
    const padding = 15;

    const messageSize: f32 = rl.measureTextEx(self.font.*, self.text, config.buttonFontSize, config.textSpacing).x;
    const messageX = self.textPosition.x;
    const floatFont: f32 = @floatFromInt(config.buttonFontSize);
    const messageY = self.textPosition.y + self.textSize.y / 2 - floatFont / 2;
    rl.drawTextEx(
        self.font.*,
        self.text,
        .{ .x = messageX, .y = messageY },
        config.buttonFontSize,
        config.textSpacing,
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
    _ = std.fmt.bufPrint(&buf, "{}", .{self.current.*}) catch unreachable;
    const currentMessageSize: f32 = rl.measureTextEx(self.font.*, &buf, config.buttonFontSize, config.textSpacing).x;
    const currentMessageX = minusPosition.x + padding + attrButtonSize.x;
    rl.drawTextEx(
        self.font.*,
        &buf,
        .{ .x = currentMessageX, .y = messageY },
        config.buttonFontSize,
        config.textSpacing,
        self.textColor,
    );

    const plusPosition: rl.Vector2 = .{
        .x = currentMessageX + currentMessageSize + padding,
        .y = messageY,
    };

    const plusButton = Button.Clickable{
        .font = self.font,
        .sound = self.sound,
    };
    if (plusButton.at(
        "+",
        plusPosition,
        attrButtonSize,
        config.ColorPalette.primary,
    )) self.current.* +|= 1;

    const minusButton = Button.Clickable{
        .font = self.font,
        .sound = self.sound,
    };
    if (minusButton.at(
        "-",
        minusPosition,
        attrButtonSize,
        config.ColorPalette.primary,
    )) self.current.* -|= 1;
}
