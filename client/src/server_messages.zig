const erl = @import("erlang/config.zig");
const std = @import("std");
const rl = @import("raylib");
const sender = @import("erlang/sender.zig");
const receiver = @import("erlang/receiver.zig");

pub const Action = enum {
    user_registry,
    user_login,
    debug,
};

pub const Erlang_Response = union(enum) {
    ok: void,
    @"error": [:0]const u8,
};

pub const Erlang_Character = struct {
    name: [:0]const u8,
    constitution: i64,
    wisdom: i64,
    endurance: i64,
    strength: i64,
    intelligence: i64,
    faith: i64,
};

pub const Character = struct {
    character_data: Erlang_Character,
    equipment_data: rl.Texture2D,
};

pub const Erlang_Characters = union(enum) {
    ok: []const Erlang_Character,
    @"error": [:0]const u8,
};

pub const User_Registry = struct {
    username: [:0]const u8,
    email: [:0]const u8,
    password: [:0]const u8,
};

pub const User_Login = struct {
    username: [:0]const u8,
    password: [:0]const u8,
};

pub const Payload = union(Action) {
    user_registry: User_Registry,
    user_login: User_Login,
    debug: [:0]const u8,
};

fn send_user_registry(ec: *erl.Node, message: User_Registry) !void {
    return sender.run_with_self(ec, .{ .map = &.{
        .{ .{ .atom = "action" }, .{ .atom = "registration" } },
        .{ .{ .atom = "email" }, .{ .string = message.email } },
        .{ .{ .atom = "username" }, .{ .string = message.username } },
        .{ .{ .atom = "password" }, .{ .string = message.password } },
    } });
}

fn send_user_login(ec: *erl.Node, message: User_Login) !void {
    return sender.run_with_self(ec, .{ .map = &.{
        .{ .{ .atom = "action" }, .{ .atom = "login" } },
        .{ .{ .atom = "username" }, .{ .string = message.username } },
        .{ .{ .atom = "password" }, .{ .string = message.password } },
    } });
}

pub fn send_payload(ec: *erl.Node, message: Payload) !void {
    switch (message) {
        .user_registry => |item| {
            try send_user_registry(ec, item);
        },
        .user_login => |item| {
            try send_user_login(ec, item);
        },
        .debug => |item| {
            try sender.run_with_self(ec, .{
                .atom = item,
            });
        },
    }
}

pub fn receive_simple_response(allocator: std.mem.Allocator, ec: *erl.Node) !Erlang_Response {
    const simple_type = receiver.With_Pid(Erlang_Response);
    const response = try receiver.run(simple_type, allocator, ec);
    return response.@"1";
}

pub fn receive_characters_list(allocator: std.mem.Allocator, ec: *erl.Node) !Erlang_Characters {
    const characters_type = receiver.With_Pid(Erlang_Characters);
    const response = try receiver.run(characters_type, allocator, ec);
    return response.@"1";
}
