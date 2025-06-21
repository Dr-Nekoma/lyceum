const config = @import("../config.zig");
const rl = @import("raylib");

pub const menuTextBoxSize: rl.Vector2 = .{
    .x = 200,
    .y = 41,
};

content: [:0]u8,
position: *usize,

pub fn at(
    self: @This(),
    textBoxPosition: rl.Vector2,
    textBoxSize: rl.Vector2,
    font: *rl.Font,
) void {
    return self.at_impl(0, textBoxPosition, textBoxSize, font);
}

pub fn redacted_at(
    self: @This(),
    comptime buffer_size: usize,
    textBoxPosition: rl.Vector2,
    textBoxSize: rl.Vector2,
) void {
    return if (buffer_size == 0)
        @compileError("Buffer size for redacted text must be non-zero")
    else
        self.at_impl(buffer_size, textBoxPosition, textBoxSize);
}

fn pushCharacters(self: @This()) void {
    const key = rl.getCharPressed();

    if ((key >= 32) and (key <= 125) and (self.position.* < self.content.len)) {
        const castKey: u32 = @bitCast(key);
        self.content[self.position.*] = @truncate(castKey);
        self.content[self.position.* + 1] = 0;
        self.position.* += 1;
    }

    if (rl.isKeyPressed(.backspace) and self.position.* > 0) {
        self.position.* -= 1;
        if (self.position.* < 0) self.position.* = 0;
        self.content[self.position.*] = 0;
    }
}

fn drawTextBox(position: rl.Vector2, size: rl.Vector2, color: rl.Color) void {
    rl.drawRectangleV(position, size, config.ColorPalette.primary);
    rl.drawRectangleLines(
        @intFromFloat(position.x),
        @intFromFloat(position.y),
        @intFromFloat(size.x),
        @intFromFloat(size.y),
        color,
    );
}

fn drawText(content: [:0]u8, position: rl.Vector2, size: rl.Vector2, font: *rl.Font) void {
    return rl.drawTextEx(
        font.*,
        content,
        .{
            .x = position.x + 5,
            .y = position.y + size.y / 2 - config.textFontSize / 2 - 2,
        },
        config.textFontSize,
        config.textSpacing,
        config.ColorPalette.secondary,
    );
}

pub fn chat(
    self: @This(),
    textBoxPosition: rl.Vector2,
    textBoxSize: rl.Vector2,
    font: *rl.Font,
) void {
    pushCharacters(self);
    drawTextBox(textBoxPosition, textBoxSize, config.ColorPalette.secondary);
    drawText(self.content, textBoxPosition, textBoxSize, font);
}

fn at_impl(
    self: @This(),
    comptime buffer_size: usize,
    initialTextBoxPosition: rl.Vector2,
    textBoxSize: rl.Vector2,
    font: *rl.Font,
) void {
    const textBoxPosition: rl.Vector2 = .{ .x = initialTextBoxPosition.x - (textBoxSize.x / 2), .y = initialTextBoxPosition.y };
    const textBox = rl.Rectangle.init(
        textBoxPosition.x,
        textBoxPosition.y,
        textBoxSize.x,
        textBoxSize.y,
    );
    const mouseOnText = blk: {
        if (rl.checkCollisionPointRec(rl.getMousePosition(), textBox)) {
            rl.setMouseCursor(.ibeam);
            pushCharacters(self);
            break :blk true;
        } else {
            rl.setMouseCursor(.default);
            break :blk false;
        }
    };

    drawTextBox(textBoxPosition, textBoxSize, if (mouseOnText) config.ColorPalette.secondary else config.ColorPalette.secondary);

    const is_redacted = buffer_size != 0;
    var redaction: [buffer_size:0]u8 = .{0} ** buffer_size;
    if (is_redacted)
        @memset(redaction[0..@min(self.content.len, buffer_size)], '*');

    drawText(if (is_redacted) &redaction else self.content, textBoxPosition, textBoxSize, font);

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
