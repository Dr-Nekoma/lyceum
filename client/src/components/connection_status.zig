const config = @import("../config.zig");
const rl = @import("raylib");

const standardSize: rl.Vector2 = .{
    .x = 200,
    .y = 30,
};

const iconSize: rl.Vector2 = .{
    .x = 50,
    .y = 50,
};

icons: struct {
    connected: *rl.Texture2D,
    not_connected: *rl.Texture2D,
},

const statusLabelPositionX = 40;

pub fn at(
    self: @This(),
    status: bool,
    height: f32,
) void {
    const position = .{
        .x = 0,
        .y = height - standardSize.y,
    };
    const iconPosition = .{
        .x = 1,
        .y = height - standardSize.y,
    };
    const icon = if (status) self.icons.not_connected.* else self.icons.connected.*;
    const statusLabel = if (status) "Connected" else "Not Connected";

    const statusLabelPositionY = height - standardSize.y + config.menuButtonsPadding;

    rl.drawRectangleV(position, standardSize, config.ColorPalette.connection_status);
    rl.drawText(
        statusLabel,
        @intFromFloat(statusLabelPositionX),
        @intFromFloat(statusLabelPositionY),
        config.textFontSize,
        rl.Color.white,
    );
    rl.drawTextureEx(icon, iconPosition, 0.0, 0.16, rl.Color.white);
}
