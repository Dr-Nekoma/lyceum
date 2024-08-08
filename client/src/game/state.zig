const erl = @import("../erlang.zig");
const rl = @import("raylib");
const messages = @import("../server_messages.zig");
const std = @import("std");
const mainMenu = @import("../menu/main.zig");

pub const Scene = enum {
    user_registry,
    user_login,
    nothing,
    join,
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
camera: rl.Camera,
test_model: ?rl.Model = null,
model_position: rl.Vector3 = .{
    .x = 0.0,
    .y = 16.0,
    .z = 0.0,
},
// TODO: Change the name and maybe even the location of this
test_value: usize = 0,
cameraDistance: f32 = 60,

pub fn init(width: f32, height: f32, node: *erl.Node) !@This() {
    const camera: rl.Camera = .{
        .position = .{ .x = 50.0, .y = 50.0, .z = 50.0 },
        .target = .{ .x = 0.0, .y = 10.0, .z = 0.0 },
        .up = .{ .x = 0.0, .y = 1.0, .z = 0.0 },
        .fovy = 45.0,
        .projection = .camera_perspective,
    };
    if (width < 0) return error.negative_width;
    if (height < 0) return error.negative_height;
    return .{
        .width = width,
        .height = height,
        .node = node,
        .camera = camera,
    };
}
