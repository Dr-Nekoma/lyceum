const erl = @import("../erlang.zig");
const messages = @import("../server_messages.zig");
const std = @import("std");
const mainMenu = @import("../menu/main.zig");

pub const Scene = enum {
    user_registry,
    user_login,
    nothing,
    spawn,
    character_selection,
};

scene: Scene = .nothing,
width: f32,
height: f32,
menu: mainMenu.Menu = undefined,
node: *erl.Node,
allocator: std.mem.Allocator = std.heap.c_allocator,
current_character: messages.Erlang_Character = .{},
character_list: []const messages.Character = &.{},
test_value: usize = 0,

pub fn init(width: f32, height: f32, node: *erl.Node) !@This() {
    if (width < 0) return error.negative_width;
    if (height < 0) return error.negative_height;
    return .{
        .width = width,
        .height = height,
        .node = node,
    };
}
