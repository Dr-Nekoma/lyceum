const config = @import("../config.zig");
const rl = @import("raylib");
const std = @import("std");
const GameState = @import("../game/state.zig");

fn highlighting(position: rl.Vector2, size: rl.Vector2, offset: f32) void {
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

pub const Sizes = struct {
    pub fn extraLarge(gameState: *const GameState) rl.Vector2 {
        return .{
            .x = gameState.width / 4,
            .y = gameState.height / 8,
        };
    }

    pub fn large(gameState: *const GameState) rl.Vector2 {
        return .{
            .x = gameState.width / 4,
            .y = gameState.height / 10,
        };
    }

    pub fn medium(gameState: *const GameState) rl.Vector2 {
        return .{
            .x = (gameState.width / 5),
            .y = (gameState.height / 11),
        };
    }

    pub fn small(gameState: *const GameState) rl.Vector2 {
        return .{
            .x = (gameState.width / 6),
            .y = (gameState.height / 12),
        };
    }

    pub fn tiny(gameState: *const GameState) rl.Vector2 {
        return .{
            .x = (gameState.width / 9),
            .y = (gameState.height / 15),
        };
    }
};

pub const Clickable = struct {
    disabled: bool = false,
    pub const Back = struct {
        fn draw(height: f32) bool {
            const size: rl.Vector2 = .{
                .x = height / 10,
                .y = height / 10,
            };
            const position: rl.Vector2 = .{
                .x = height / 40,
                .y = height / 20,
            };
            const buttonArea = rl.Rectangle.init(
                position.x,
                position.y,
                size.x,
                size.y,
            );
            const isHovered = rl.checkCollisionPointRec(
                rl.getMousePosition(),
                buttonArea,
            );
            if (isHovered) {
                const offset: f32 = 5.0;
                highlighting(position, size, offset);
            } else if (isHovered) {
                const time = rl.getTime();
                const offset: f32 = @floatCast(2 * @sin(2 * time) + 3);
                highlighting(position, size, offset);
            }
            rl.drawRectangleV(position, size, config.ColorPalette.primary);

            const triangle_top: rl.Vector2 = .{
                .x = 2 * size.x / 3 + position.x,
                .y = size.y / 6 + position.y,
            };
            const triangle_bottom: rl.Vector2 = .{
                .x = 2 * size.x / 3 + position.x,
                .y = 5 * size.y / 6 + position.y,
            };
            const triangle_left: rl.Vector2 = .{
                .x = size.x / 3 + position.x,
                .y = size.y / 2 + position.y,
            };

            rl.drawTriangle(
                triangle_top,
                triangle_left,
                triangle_bottom,
                rl.Color.white,
            );

            return isHovered;
        }

        pub fn at(
            scene: *GameState.Scene,
            height: f32,
        ) void {
            var isHovered = false;
            switch (scene.*) {
                .nothing, .spawn => return,
                else => isHovered = draw(height),
            }
            if (isHovered and rl.isMouseButtonPressed(.mouse_button_left)) {
                scene.* = switch (scene.*) {
                    .user_registry, .user_login, .nothing, .join, .spawn, .character_selection, .connect => .nothing,
                };
            }
        }
    };

    pub fn at(
        self: @This(),
        message: [:0]const u8,
        position: rl.Vector2,
        size: rl.Vector2,
        activeColor: rl.Color,
    ) bool {
        const buttonArea = rl.Rectangle.init(
            position.x,
            position.y,
            size.x,
            size.y,
        );
        const isHovered = rl.checkCollisionPointRec(
            rl.getMousePosition(),
            buttonArea,
        );
        var color = activeColor;
        if (!self.disabled) {
            if (isHovered) {
                const time = rl.getTime();
                const offset: f32 = @floatCast(2 * @sin(2 * time) + 3);
                highlighting(position, size, offset);
            }
        } else {
            color = config.ColorPalette.disabled;
        }
        rl.drawRectangleV(position, size, color);
        const messageSize: f32 = @floatFromInt(rl.measureText(message, config.buttonFontSize));
        const messageX = position.x + size.x / 2 - messageSize / 2;
        const floatFont: f32 = @floatFromInt(config.buttonFontSize);
        const messageY = position.y + size.y / 2 - floatFont / 2;
        rl.drawText(
            message,
            @intFromFloat(messageX),
            @intFromFloat(messageY),
            config.buttonFontSize,
            config.ColorPalette.secondary,
        );
        return !self.disabled and (isHovered and rl.isMouseButtonPressed(.mouse_button_left));
    }
};

pub const Selectable = struct {
    index: ?usize = null,
    pub fn at(
        self: *@This(),
        message: [:0]const u8,
        position: rl.Vector2,
        size: rl.Vector2,
        color: rl.Color,
        currentIndex: ?usize,
    ) bool {
        const buttonArea = rl.Rectangle.init(
            position.x,
            position.y,
            size.x,
            size.y,
        );
        const isHovered = rl.checkCollisionPointRec(
            rl.getMousePosition(),
            buttonArea,
        );
        const isSelected = currentIndex == self.index;
        if (isSelected) {
            const offset: f32 = 5.0;
            highlighting(position, size, offset);
        } else if (isHovered) {
            const time = rl.getTime();
            const offset: f32 = @floatCast(2 * @sin(2 * time) + 3);
            highlighting(position, size, offset);
        }
        rl.drawRectangleV(position, size, color);
        const messageSize: f32 = @floatFromInt(rl.measureText(message, config.buttonFontSize));
        const messageX = position.x + size.x / 2 - messageSize / 2;
        const floatFont: f32 = @floatFromInt(config.buttonFontSize);
        const messageY = position.y + size.y / 2 - floatFont / 2;
        rl.drawText(
            message,
            @intFromFloat(messageX),
            @intFromFloat(messageY),
            config.buttonFontSize,
            config.ColorPalette.secondary,
        );

        const clicked = isHovered and rl.isMouseButtonPressed(.mouse_button_left);
        return clicked or isSelected;
    }
};

pub const SelectableGroup = struct {
    selected: ?usize = null,
    instances: [config.maximumCharacters]Selectable = .{Selectable{}} ** config.maximumCharacters,
};
