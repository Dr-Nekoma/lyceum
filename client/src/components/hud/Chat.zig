const common = @import("common.zig");
const GameState = @import("../../game/state.zig");
const rl = @import("raylib");
const std = @import("std");
const text = @import("../text.zig");

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

messages: std.ArrayList(Message),
content: [:0]u8,
position: *usize,

pub fn at(
    character: *GameState.World.Character,
    width: f32,
    height: f32,
) !void {
    var chat = &character.inventory.hud.chat;
    const boundarySize: rl.Vector2 = .{
        .x = width / 5,
        .y = height / 5,
    };
    const boundaryPosition: rl.Vector2 = .{
        .x = width - boundarySize.x,
        .y = 0,
    };

    rl.drawRectangleV(boundaryPosition, boundarySize, textBoxColor);
    if (chat.mode == .writing) {
        const textElem = text{
            .content = chat.in.content,
            .position = chat.in.position,
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
        );
    } else if (chat.mode == .idle and chat.in.position.* > 0) {
        const message: Message = .{
            .author = character.name,
            .content = try std.heap.page_allocator.allocSentinel(u8, chat.in.position.*, 0),
        };
        std.mem.copyForwards(u8, message.content, chat.in.content[0..chat.in.position.*]);
        try chat.in.messages.append(message);
        @memset(chat.in.content, 0);
        chat.in.position.* = 0;
    }

    if (rl.isKeyDown(.key_enter)) {
        if (chat.mode == .idle) {
            chat.mode = .writing;
        } else if (chat.mode == .writing) {
            chat.mode = .idle;
            if (chat.in.messages.items.len > 0) {
                std.debug.print("Adding message to chat is yet not implemented xd\n", .{});
            }
        }
    }
}
