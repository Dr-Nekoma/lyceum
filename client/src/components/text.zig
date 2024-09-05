const config = @import("../config.zig");
const rl = @import("raylib");

pub const textBoxSize: rl.Vector2 = .{
    .x = 200,
    .y = 41,
};

content: [:0]u8,
position: *usize,

pub fn at(
    self: @This(),
    textBoxPosition: rl.Vector2,
) void {
    return self.at_impl(0, textBoxPosition);
}

pub fn redacted_at(
    self: @This(),
    comptime buffer_size: usize,
    textBoxPosition: rl.Vector2,
) void {
    return if (buffer_size == 0)
        @compileError("Buffer size for redacted text must be non-zero")
    else
        self.at_impl(buffer_size, textBoxPosition);
}

fn at_impl(
    self: @This(),
    comptime buffer_size: usize,
    initialTextBoxPosition: rl.Vector2,
) void {
    const textBoxPosition: rl.Vector2 = .{ .x = initialTextBoxPosition.x - (textBoxSize.x / 2), .y = initialTextBoxPosition.y };
    const textBox = rl.Rectangle.init(
        textBoxPosition.x,
        textBoxPosition.y,
        textBoxSize.x,
        textBoxSize.y,
    );
    var mouseOnText = false;
    if (rl.checkCollisionPointRec(rl.getMousePosition(), textBox)) {
        mouseOnText = true;

        rl.setMouseCursor(.mouse_cursor_ibeam);

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
        rl.setMouseCursor(.mouse_cursor_default);
    }

    rl.drawRectangleV(textBoxPosition, textBoxSize, config.ColorPalette.primary);
    rl.drawRectangleLines(
        @intFromFloat(textBoxPosition.x),
        @intFromFloat(textBoxPosition.y),
        @intFromFloat(textBoxSize.x),
        @intFromFloat(textBoxSize.y),
        if (mouseOnText) config.ColorPalette.secondary else rl.Color.white,
    );

    const is_redacted = buffer_size != 0;
    var redaction: [buffer_size:0]u8 = .{0} ** buffer_size;
    if (is_redacted)
        @memset(redaction[0..@min(self.content.len, buffer_size)], '*');

    rl.drawText(
        if (is_redacted) &redaction else self.content,
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
