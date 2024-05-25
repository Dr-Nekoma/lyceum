const rl = @import("raylib");
const config = @import("../config.zig");

pub fn at(message: [:0]const u8, position: rl.Vector2, size: rl.Vector2, color: rl.Color) bool {
    const buttonArea = rl.Rectangle.init(position.x, position.y, size.x, size.y);
    const isSelected = rl.checkCollisionPointRec(rl.getMousePosition(), buttonArea);
    if (isSelected) {
        const time = rl.getTime();
        const offset: f32 = @floatCast(2 * @sin(2 * time) + 3);
        const boundaryColor: rl.Color = rl.Color.init(255, 255, 255, 127);
        const boundaryPosition: rl.Vector2 = .{
            .x = position.x - offset,
            .y = position.y - offset,
        };
        const boundarySize: rl.Vector2 = .{
            .x = size.x + 2 * offset,
            .y = size.y + 2 * offset,
        };
        rl.drawRectangleV(boundaryPosition, boundarySize, boundaryColor);
    }
    rl.drawRectangleV(position, size, color);
    const messageSize: f32 = @floatFromInt(rl.measureText(message, config.buttonFontSize));
    const messageX = position.x + size.x / 2 - messageSize / 2;
    const floatFont: f32 = @floatFromInt(config.buttonFontSize);
    const messageY = position.y + size.y / 2 - floatFont / 2;
    rl.drawText(message, @intFromFloat(messageX), @intFromFloat(messageY), config.buttonFontSize, config.ColorPalette.secondary);
    return (isSelected and rl.isMouseButtonPressed(.mouse_button_left));
}
