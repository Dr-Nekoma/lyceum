const erl = @import("erlang/config.zig");
const std = @import("std");
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
