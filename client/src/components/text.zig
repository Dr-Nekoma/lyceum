const rl = @import("raylib");
const config = @import("../config.zig");

pub const textBoxSize: rl.Vector2 = .{
    .x = 200,
    .y = 41,
};

content: [:0]u8,
position: *usize,

pub fn at(
    self: @This(),
    textBoxPosition: rl.Vector2,
    _: bool,
) void {
    const textBox = rl.Rectangle.init(
        textBoxPosition.x,
        textBoxPosition.y,
        textBoxSize.x,
        textBoxSize.y,
    );
    var mouseOnText = false;
    if (rl.checkCollisionPointRec(rl.getMousePosition(), textBox)) {
        mouseOnText = true;

        rl.setMouseCursor(@intFromEnum(rl.MouseCursor.mouse_cursor_ibeam));

        var key = rl.getCharPressed();

        while (key > 0) {
            if ((key >= 32) and (key <= 125) and (self.position.* < self.content.len)) {
                const castKey: u32 = @bitCast(key);
                self.content[self.position.*] = @truncate(castKey);
                self.content[self.position.* + 1] = 0;
                self.position.* += 1;
            }

            key = rl.getCharPressed();
        }

        if (rl.isKeyPressed(.key_backspace)) {
            self.position.* -= 1;
            if (self.position.* < 0) self.position.* = 0;
            self.content[self.position.*] = 0;
        }
    } else {
        mouseOnText = false;
        rl.setMouseCursor(@intFromEnum(rl.MouseCursor.mouse_cursor_default));
    }

    rl.drawRectangleV(textBoxPosition, textBoxSize, config.ColorPalette.primary);
    if (mouseOnText) {
        rl.drawRectangleLines(
            @intFromFloat(textBoxPosition.x),
            @intFromFloat(textBoxPosition.y),
            @intFromFloat(textBoxSize.x),
            @intFromFloat(textBoxSize.y),
            config.ColorPalette.secondary,
        );
    } else {
        rl.drawRectangleLines(
            @intFromFloat(textBoxPosition.x),
            @intFromFloat(textBoxPosition.y),
            @intFromFloat(textBoxSize.x),
            @intFromFloat(textBoxSize.y),
            rl.Color.white,
        );
    }

    // TODO: Fix redacted drawing buffer
    // const redactedBuffer = if (!redacted) self.content else (.{'*'} ** self.position) ++ .{0};
    rl.drawText(
        self.content,
        @intFromFloat(textBoxPosition.x + 5),
        @intFromFloat(textBoxPosition.y + 8),
        config.textFontSize,
        rl.Color.white,
    );

    // TODO: Fix blinking cursor
    // if (mouseOnText) {
    //     if (self.position < formFieldsBufferSize) {
    //         // Draw blinking underscore char
    //         if (((framesCounter/20)%2) == 0) DrawText("_", (int)textBox.x + 8 + MeasureText(self.content, 40), (int)textBox.y + 12, 40, MAROON);
    //     } else {
    //         DrawText("Press BACKSPACE to delete chars...", 230, 300, 20, GRAY);
    //     }
    // }
}
