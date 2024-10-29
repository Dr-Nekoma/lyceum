const config = @import("../config.zig");
const rl = @import("raylib");

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
    const icon, const statusLabel = if (status) .{ self.icons.not_connected.*, "Connected" } else .{ self.icons.connected.*, "Not Connected" };
    const statusLabelLength: f32 = @floatFromInt(rl.measureText(statusLabel, config.textFontSize));

    const size: rl.Vector2 = .{
        .x = 50 + statusLabelLength,
        .y = 30,
    };

    const position = .{
        .x = 0,
        .y = height - size.y,
    };

    const iconPosition = .{
        .x = 1,
        .y = height - size.y,
    };

    const statusLabelPositionY = height - size.y + config.menuButtonsPadding;

    rl.drawRectangleV(position, size, config.ColorPalette.connection_status);
    rl.drawText(
        statusLabel,
        @intFromFloat(statusLabelPositionX),
        @intFromFloat(statusLabelPositionY),
        config.textFontSize,
        rl.Color.white,
    );
    rl.drawTextureEx(icon, iconPosition, 0.0, 0.16, rl.Color.white);
}
