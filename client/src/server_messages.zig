const erl = @import("erlang/config.zig");
const std = @import("std");
const rl = @import("raylib");
const sender = @import("erlang/sender.zig");
const receiver = @import("erlang/receiver.zig");

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

pub const Payload = union(enum) {
    register: User_Registry,
    login: User_Login,
    character_list: User_Characters_Request,
    debug: [:0]const u8,
};

pub fn send_payload(ec: *erl.Node, message: Payload) !void {
    try sender.run(ec, message);
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
