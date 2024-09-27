const config = @import("../../config.zig");
const rl = @import("raylib");

pub const slotBoxSize: rl.Vector2 = .{
    .x = 90,
    .y = 90,
};

pub const slotInternalSize: rl.Vector2 = .{
    .x = slotBoxSize.x * 4 / 5,
    .y = slotBoxSize.y * 4 / 5,
};

const textBoxSize: rl.Vector2 = .{
    .x = config.hubFontSize + 4,
    .y = config.hubFontSize + 6,
};

pub const internalPadding = 5;

pub fn drawBoundary(position: rl.Vector2) void {
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

pub fn drawSlot(xPosition: f32, yPosition: f32, label: [:0]const u8) void {
    rl.drawRectangleV(.{
        .x = xPosition,
        .y = yPosition,
    }, slotInternalSize, rl.Color.blue);
    rl.drawRectangleV(.{
        .x = xPosition,
        .y = yPosition,
    }, textBoxSize, rl.Color.black);
    rl.drawText(label, @intFromFloat(xPosition), @intFromFloat(yPosition), config.hubFontSize, rl.Color.white);
}
