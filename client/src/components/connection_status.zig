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
    font: *rl.Font,
) void {
    const icon, const statusLabel = if (status) .{ self.icons.connected.*, "Connected" } else .{ self.icons.not_connected.*, "Not Connected" };
    const statusLabelLength = rl.measureTextEx(font.*, statusLabel, config.textFontSize, config.textSpacing);

    const size: rl.Vector2 = .{
        .x = 50 + statusLabelLength.x,
        .y = 10 + statusLabelLength.y,
    };

    const position = .{
        .x = 0,
        .y = height - size.y,
    };

    const iconPosition = .{
        .x = 1,
        .y = height - size.y,
    };

    const statusLabelPositionY = height - size.y + 5;

    rl.drawRectangleV(position, size, config.ColorPalette.connection_status);
    rl.drawTextEx(
        font.*,
        statusLabel,
        .{ .x = statusLabelPositionX, .y = statusLabelPositionY },
        config.textFontSize,
        config.textSpacing,
        rl.Color.white,
    );
    rl.drawTextureEx(icon, iconPosition, 0.0, 0.16, rl.Color.white);
}
