const erl = @import("erlang/config.zig");
const std = @import("std");
const rl = @import("raylib");
const sender = @import("erlang/sender.zig");
const receiver = @import("erlang/receiver.zig");
pub const ei = @cImport({
    @cInclude("ei.h");
});

pub const Erlang_Response = union(enum) {
    ok: void,
    @"error": [:0]const u8,
};

pub const Login_Response = union(enum) {
    ok: [:0]const u8,
    @"error": [:0]const u8,
};

pub fn Tuple_Response(comptime T: type) type {
    return union(enum) {
        ok: T,
        @"error": [:0]const u8,
    };
}

pub const Erlang_Character = struct {
    name: [:0]const u8 = "",
    constitution: u8 = 0,
    wisdom: u8 = 0,
    endurance: u8 = 0,
    strength: u8 = 0,
    intelligence: u8 = 0,
    faith: u8 = 0,
    x_position: u64 = 0,
    y_position: u64 = 0,
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

pub const Character_Update = struct {
    character_data: Erlang_Character,
    user_info: User_Characters_Request,
};

pub const Payload = union(enum) {
    user_registry: User_Registry,
    user_login: User_Login,
    character_list: User_Characters_Request,
    character_update: Character_Update,
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
    return sender.run(ec, .{ .map = &.{
        .{ .{ .atom = "action" }, .{ .atom = "character_list" } },
        .{ .{ .atom = "username" }, .{ .string = message.username } },
        .{ .{ .atom = "email" }, .{ .string = message.email } },
    } });
}

fn send_character_update(ec: *erl.Node, message: Character_Update) !void {
    return sender.run(ec, .{ .map = &.{
        .{ .{ .atom = "action" }, .{ .atom = "character_update" } },
        .{ .{ .atom = "username" }, .{ .string = message.user_info.username } },
        .{ .{ .atom = "name" }, .{ .string = message.character_data.name } },
        .{ .{ .atom = "email" }, .{ .string = message.user_info.email } },
        .{ .{ .atom = "map_name" }, .{ .string = message.character_data.map_name } },
        .{ .{ .atom = "x_position" }, .{ .number = .{ .eu64 = message.character_data.x_position } } },
        .{ .{ .atom = "y_position" }, .{ .number = .{ .eu64 = message.character_data.y_position } } },
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
        .character_update => |item| {
            try send_character_update(ec, item);
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
    return try receiver.run(Erlang_Characters, allocator, ec);
}

const handlerWithEmail = std.meta.Tuple(&.{ ei.erlang_pid, [:0]const u8 });
pub fn receive_handler_and_email(allocator: std.mem.Allocator, ec: *erl.Node) !handlerWithEmail {
    const handler_email_type = Tuple_Response(handlerWithEmail);
    const response = try receiver.run(handler_email_type, allocator, ec);
    switch (response) {
        .ok => |item| return item,
        .@"error" => |msg| {
            std.debug.print("There was an error: {s}", .{msg});
            return error.did_not_get_handler_pid;
        },
    }
}
