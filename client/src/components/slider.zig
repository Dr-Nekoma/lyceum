const config = @import("../config.zig");
const rl = @import("raylib");

position: rl.Vector2,
size: rl.Vector2,
outterColor: rl.Color,
innerColor: rl.Color,
ceiling: u16 = 999,
current: *u16,

pub fn at(
    self: @This(),
) void {
    const innerButtonArea = rl.Rectangle.init(
        self.position.x + 5,
        self.position.y + 5,
        self.size.x - 5,
        self.size.y - 10,
    );
    const mousePosition = rl.getMousePosition();
    const isSelected = rl.checkCollisionPointRec(
        mousePosition,
        innerButtonArea,
    );
    const float_ceiling: f32 = @floatFromInt(self.ceiling);
    if (isSelected and rl.isMouseButtonPressed(.mouse_button_left)) {
        self.current.* = @intFromFloat(((mousePosition.x - innerButtonArea.x) / innerButtonArea.width) * float_ceiling);
    }
    const boundaryPosition: rl.Vector2 = .{
        .x = innerButtonArea.x,
        .y = innerButtonArea.y,
    };
    const float_current: f32 = @floatFromInt(self.current.*);
    const boundarySize: rl.Vector2 = .{
        .x = (float_current / float_ceiling) * innerButtonArea.width,
        .y = innerButtonArea.height,
    };
    rl.drawRectangleV(self.position, self.size, self.outterColor);
    rl.drawRectangleV(boundaryPosition, boundarySize, self.innerColor);
}
