const rl = @import("raylib");
const std = @import("std");
const zerl = @import("zerl");
const GameState = @import("game/state.zig");

fn createAnonymousStruct(comptime T: type, comptime keys: []const [:0]const u8) type {
    const struct_info = @typeInfo(T).Struct;
    comptime var structKeys: [keys.len]std.meta.Tuple(&.{[:0]const u8}) = undefined;
    comptime for (0.., keys) |index, key| {
        structKeys[index] = .{key};
    };
    const mapKeys = std.StaticStringMap(void).initComptime(structKeys);
    return comptime blk: {
        var fields: [keys.len]std.builtin.Type.StructField = undefined;
        var fieldsCounter: usize = 0;
        for (struct_info.fields) |field| {
            if (mapKeys.has(field.name)) {
                fields[fieldsCounter] = field;
                fieldsCounter += 1;
            }
        }
        break :blk @Type(.{
            .Struct = .{
                .layout = .auto,
                .fields = &fields,
                .decls = &[_]std.builtin.Type.Declaration{},
                .is_tuple = false,
            },
        });
    };
}

pub fn selectKeysFromStruct(data: anytype, comptime keys: []const [:0]const u8) createAnonymousStruct(@TypeOf(data), keys) {
    const Temp: type = createAnonymousStruct(@TypeOf(data), keys);
    var anonymousStruct: Temp = undefined;
    inline for (@typeInfo(Temp).Struct.fields) |field| {
        const current_field = &@field(anonymousStruct, field.name);
        current_field.* = @field(data, field.name);
    }
    return anonymousStruct;
}

// Standard Response from Erlang Server

fn Tuple_Response(comptime T: type) type {
    return union(enum) {
        ok: T,
        @"error": [:0]const u8,
    };
}

pub const Erlang_Response = Tuple_Response(void);

// User's Login and Registration

pub const Login_Request = struct {
    username: []const u8,
    password: []const u8,
};

pub const Login_Info = std.meta.Tuple(&.{ zerl.ei.erlang_pid, [:0]const u8 });
const Login_Response = Tuple_Response(Login_Info);

pub const Registry_Request = struct {
    username: [:0]const u8,
    email: [:0]const u8,
    password: [:0]const u8,
};

// TODO: Implement user registration via the client
pub const Registry_Response = Tuple_Response(void);

// User's Characters

pub const Character_Info = struct {
    name: [:0]const u8 = "",
    constitution: u8 = 0,
    wisdom: u8 = 0,
    endurance: u8 = 0,
    strength: u8 = 0,
    intelligence: u8 = 0,
    faith: u8 = 0,
    x_position: i16 = 0,
    y_position: i16 = 0,
    x_velocity: f32 = 0,
    y_velocity: f32 = 0,
    face_direction: u16 = 270,
    map_name: [:0]const u8 = "",
    state_type: GameState.World.Character.Animation.State = .idle,
};

pub const Characters_Request = struct {
    username: []const u8,
    email: []const u8,
};

pub const Characters_Response = Tuple_Response([]const Character_Info);

pub const Character_Update = struct {
    name: [:0]const u8,
    x_position: i16,
    y_position: i16,
    x_velocity: f32,
    y_velocity: f32,
    map_name: [:0]const u8,
    username: []const u8,
    face_direction: u16 = 270,
    email: []const u8,
    state_type: GameState.World.Character.Animation.State = .idle,
};

// Central place to send game's data

pub const Payload = union(enum) {
    register: Registry_Request,
    login: Login_Request,
    list_characters: Characters_Request,
    // create_character:
    joining_map: Character_Update,
    update_character: Character_Update,
    exit_map: void,
    debug: [:0]const u8,
};

// Central place to receive game's data

pub fn receive_standard_response(allocator: std.mem.Allocator, ec: *zerl.Node) !Erlang_Response {
    return ec.receive(Erlang_Response, allocator);
}

pub fn receive_login_response(allocator: std.mem.Allocator, ec: *zerl.Node) !Login_Info {
    const response = try ec.receive(Login_Response, allocator);
    switch (response) {
        .ok => |item| {
            return item;
        },
        .@"error" => |msg| {
            defer allocator.free(msg);
            std.debug.print("[ERROR]: {s}\n", .{msg});
            return error.unwrapping_tuple_response;
        },
    }
}

pub fn receive_characters_list(allocator: std.mem.Allocator, ec: *zerl.Node) !Characters_Response {
    return ec.receive(Characters_Response, allocator);
}
