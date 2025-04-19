const common = @import("common.zig");
const rl = @import("raylib");
const std = @import("std");
const text = @import("../text.zig");
const GameState = @import("../../game/state.zig");

pub const Message = struct {
    author: []const u8 = "",
    content: []u8 = "",
    // TODO: Add timestamp of message
    // timestamp: something,
};

pub const Mode = enum {
    idle,
    writing,
};

const textBoxColor = rl.Color.init(0, 0, 0, 127);

messages: *std.ArrayList(Message),
content: [:0]u8,
position: *usize,
mode: *Mode,

pub fn at(
    self: @This(),
    name: [:0]const u8,
    gameState: *GameState,
) !void {
    const width: f32 = gameState.width;
    const height: f32 = gameState.height;
    const boundarySize: rl.Vector2 = .{
        .x = width / 5,
        .y = height / 5,
    };
    const boundaryPosition: rl.Vector2 = .{
        .x = width - boundarySize.x,
        .y = 0,
    };

    rl.drawRectangleV(boundaryPosition, boundarySize, textBoxColor);
    if (self.mode.* == .writing) {
        const textElem = text{
            .content = self.content,
            .position = self.position,
        };
        textElem.chat(
            .{
                .x = boundaryPosition.x,
                .y = boundarySize.y,
            },
            .{
                .x = boundarySize.x,
                .y = text.menuTextBoxSize.y,
            },
            &gameState.menu.assets.font,
        );
    } else if (self.mode.* == .idle and self.position.* > 0) {
        const message: Message = .{
            .author = name,
            .content = try gameState.allocator.allocSentinel(u8, self.position.*, 0),
        };
        std.mem.copyForwards(u8, message.content, self.content[0..self.position.*]);
        try self.messages.append(message);
        @memset(self.content, 0);
        self.position.* = 0;
    }

    if (rl.isKeyPressed(.enter)) {
        if (self.mode.* == .idle) {
            self.mode.* = .writing;
        } else if (self.mode.* == .writing) {
            self.mode.* = .idle;
            if (self.messages.items.len > 0) {
                std.debug.print("Adding message to chat is yet not implemented xd\n", .{});
            }
        }
    }
}
