const erl = @import("erlang/config.zig");
const std = @import("std");
const rl = @import("raylib");
const sender = @import("erlang/sender.zig");
const receiver = @import("erlang/receiver.zig");

pub const Action = enum {
    user_registry,
    user_login,
    character_list,
    debug,
};

pub const Erlang_Response = union(enum) {
    ok: void,
    @"error": [:0]const u8,
};

pub const Login_Response = union(enum) {
    ok: [:0]const u8,
    @"error": [:0]const u8,
};

pub const Erlang_Character = struct {
    name: [:0]const u8 = "",
    constitution: i64 = 0,
    wisdom: i64 = 0,
    endurance: i64 = 0,
    strength: i64 = 0,
    intelligence: i64 = 0,
    faith: i64 = 0,
    x_position: i64 = 0,
    y_position: i64 = 0,
    map_name: [:0]const u8 = "",
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

pub const User_Characters_Request = struct {
    username: [:0]const u8,
    email: [:0]const u8,
};

pub const Payload = union(Action) {
    user_registry: User_Registry,
    user_login: User_Login,
    character_list: User_Characters_Request,
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

fn send_character_list(ec: *erl.Node, message: User_Characters_Request) !void {
    return sender.run_with_self(ec, .{ .map = &.{
        .{ .{ .atom = "action" }, .{ .atom = "character_list" } },
        .{ .{ .atom = "username" }, .{ .string = message.username } },
        .{ .{ .atom = "email" }, .{ .string = message.email } },
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
        .character_list => |item| {
            try send_character_list(ec, item);
        },
        .debug => |item| {
            try sender.run_with_self(ec, .{
                .atom = item,
            });
        },
    }
}

pub fn receive_login_response(allocator: std.mem.Allocator, ec: *erl.Node) !Login_Response {
    const simple_type = receiver.With_Pid(Login_Response);
    const response = try receiver.run(simple_type, allocator, ec);
    return response.@"1";
}

pub fn receive_characters_list(allocator: std.mem.Allocator, ec: *erl.Node) !Erlang_Characters {
    const characters_type = receiver.With_Pid(Erlang_Characters);
    const response = try receiver.run(characters_type, allocator, ec);
    return response.@"1";
}
