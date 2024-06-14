pub const ei = @cImport({
    @cInclude("ei.h");
});

pub const std = @import("std");
pub const process_name = "lyceum_server";
pub const server_name = process_name ++ "@nixos";

pub const LNode = struct {
    c_node: ei.ei_cnode,
    fd: i32,
    node_name: [:0]const u8 = "lyceum_client",
    cookie: [:0]const u8 = "lyceum",
};

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

pub const Erlang_Data = union(enum) {
    atom: [:0]const u8,
    tuple: []const Erlang_Data,
    pid: *const ei.erlang_pid,
    map: []const [2]Erlang_Data,
    string: [:0]const u8,
};

fn erlang_validate(error_tag: anytype, result_value: c_int) !void {
    if (result_value < 0) {
        return error_tag;
    }
}

fn send_erlang_data(buf: *ei.ei_x_buff, data: Erlang_Data) !void {
    switch (data) {
        .atom => |item| {
            try erlang_validate(error.encode_atom, ei.ei_x_encode_atom(buf, item.ptr));
        },
        .tuple => |itens| {
            try erlang_validate(error.encode_tuple_header, ei.ei_x_encode_tuple_header(buf, @bitCast(itens.len)));
            for (itens) |elem| {
                try send_erlang_data(buf, elem);
            }
        },
        .pid => |pid| {
            try erlang_validate(error.encode_pid, ei.ei_x_encode_pid(buf, pid));
        },
        .map => |entries| {
            try erlang_validate(error.encode_map_header, ei.ei_x_encode_map_header(buf, @bitCast(entries.len)));
            for (entries) |entry| {
                for (entry) |value| {
                    try send_erlang_data(buf, value);
                }
            }
        },
        .string => |str| {
            try erlang_validate(error.encode_string, ei.ei_x_encode_string(buf, str.ptr));
        },
    }
}

pub fn send_message(ec: *LNode, data: Erlang_Data) !void {
    var buf: ei.ei_x_buff = undefined;
    try erlang_validate(error.new_with_version, ei.ei_x_new_with_version(&buf));
    try send_erlang_data(&buf, data);
    try erlang_validate(error.reg_send_failed, ei.ei_reg_send(&ec.c_node, ec.fd, @constCast(process_name), buf.buff, buf.index));
}

pub fn prepare_connection() !LNode {
    var l_node: LNode = .{
        .c_node = undefined,
        .fd = undefined,
    };
    const creation = std.time.timestamp() + 1;
    const creation_u: u64 = @bitCast(creation);
    const result = ei.ei_connect_init(
        &l_node.c_node,
        l_node.node_name.ptr,
        l_node.cookie.ptr,
        @truncate(creation_u),
    );
    return if (result < 0)
        error.ei_connect_init_failed
    else
        l_node;
}

pub fn establish_connection(ec: *LNode) !void {
    const sockfd = ei.ei_connect(&ec.c_node, @constCast(server_name));
    try erlang_validate(error.ei_connect_failed, sockfd);
    ec.fd = sockfd;
}

fn send_with_self(ec: *LNode, data: Erlang_Data) !void {
    return send_message(ec, .{ .tuple = &.{ .{ .pid = ei.ei_self(&ec.c_node) }, data } });
}

pub fn send_string(ec: *LNode, message: [:0]const u8) !void {
    return send_with_self(ec, .{ .atom = message });
}

fn send_user_registry(ec: *LNode, message: User_Registry) !void {
    return send_with_self(ec, .{ .map = &.{
        .{ .{ .atom = "action" }, .{ .atom = "registration" } },
        .{ .{ .atom = "email" }, .{ .string = message.email } },
        .{ .{ .atom = "username" }, .{ .string = message.username } },
        .{ .{ .atom = "password" }, .{ .string = message.password } },
    } });
}

fn send_user_login(ec: *LNode, message: User_Login) !void {
    return send_with_self(ec, .{ .map = &.{
        .{ .{ .atom = "action" }, .{ .atom = "login" } },
        .{ .{ .atom = "username" }, .{ .string = message.username } },
        .{ .{ .atom = "password" }, .{ .string = message.password } },
    } });
}

pub fn send_payload(ec: *LNode, message: Payload) !void {
    switch (message) {
        .user_registry => |item| {
            try send_user_registry(ec, item);
        },
        .user_login => |item| {
            try send_user_login(ec, item);
        },
        .debug => |item| {
            try send_with_self(ec, .{
                .atom = item,
            });
        },
    }
}

pub const Deserializer = struct {
    buf: *ei.ei_x_buff,
    index: *i32,
    ec: *LNode,
};

// TODO: Try to make the simplest possible example using something like this to report to zig repo
//pub fn receive_atom_string(deserializer: Deserializer, allocator: std.mem.Allocator, erlang_fun: fn ([*:0]const u8, *i32, [*:0]u8) callconv(.C) c_int) ![:0]const u8 {
pub fn receive_atom_string(deserializer: Deserializer, allocator: std.mem.Allocator, erlang_fun: fn ([*c]const u8, [*c]c_int, [*c]u8) callconv(.C) c_int) ![:0]const u8 {
    var length: i32 = undefined;
    var ty: i32 = undefined;
    try erlang_validate(error.decoding_atom_string_length, ei.ei_get_type(deserializer.buf.buff, deserializer.index, &ty, &length));

    if (ty != ei.ERL_STRING_EXT and ty != ei.ERL_ATOM_EXT)
        return error.message_is_not_atom_or_string;

    const u_length: u32 = @bitCast(length);

    const buffer = try allocator.allocSentinel(u8, u_length, 0);
    try erlang_validate(error.decoding_atom, erlang_fun(deserializer.buf.buff, deserializer.index, buffer.ptr));
    return buffer;
}

pub fn receive_string(deserializer: Deserializer, allocator: std.mem.Allocator) ![:0]const u8 {
    return receive_atom_string(deserializer, allocator, ei.ei_decode_string);
}

pub fn receive_atom(deserializer: Deserializer, allocator: std.mem.Allocator) ![:0]const u8 {
    return receive_atom_string(deserializer, allocator, ei.ei_decode_atom);
}

pub fn with_pid(comptime T: type) type {
    return std.meta.Tuple(&.{ ei.erlang_pid, T });
}

pub fn receive_simple_response(allocator: std.mem.Allocator, ec: *LNode) !Erlang_Response {
    const simple_type = with_pid(Erlang_Response);
    const response = try receive_message(simple_type, allocator, ec);
    return response.@"1";
}

pub fn receive_message(comptime T: type, allocator: std.mem.Allocator, ec: *LNode) !T {
    var msg: ei.erlang_msg = undefined;
    var buf: ei.ei_x_buff = undefined;
    var index: i32 = 0;

    try erlang_validate(error.create_new_decode_buff, ei.ei_x_new(&buf));

    while (true) {
        const got: i32 = ei.ei_xreceive_msg(ec.fd, &msg, &buf);
        if (got == ei.ERL_TICK)
            continue;
        if (got == ei.ERL_ERROR) {
            return error.got_error_receiving_message;
        }
        break;
    }

    try erlang_validate(error.decoding_version, ei.ei_decode_version(buf.buff, &index, null));
    return internal_receive_message(T, allocator, .{
        .buf = &buf,
        .index = &index,
        .ec = ec,
    });
}

inline fn receive_struct(comptime T: type, comptime item: std.builtin.Type.Struct, allocator: std.mem.Allocator, deserializer: Deserializer) !T {
    var value: T = undefined;
    var size: i32 = 0;
    if (item.is_tuple) {
        try erlang_validate(
            error.decoding_tuple,
            ei.ei_decode_tuple_header(deserializer.buf.buff, deserializer.index, &size),
        );
        if (item.fields.len != size) return error.wrong_tuple_size;
        inline for (&value) |*elem| {
            elem.* = try internal_receive_message(@TypeOf(elem.*), allocator, deserializer);
        }
    } else {
        try erlang_validate(
            error.decoding_map,
            ei.ei_decode_map_header(deserializer.buf.buff, deserializer.index, &size),
        );
        const fields = std.meta.fields(T);
        var present_fields: [fields.len]bool = .{false} ** fields.len;
        var counter: u32 = 0;
        if (size > fields.len) return error.too_many_map_entries;
        for (0..@as(u32, @bitCast(size))) |_| {
            const key = try receive_atom(deserializer, allocator);
            inline for (0.., fields) |idx, field| {
                if (std.mem.eql(u8, field.name, key)) {
                    const current_field = &@field(value, field.name);
                    const field_type = @typeInfo(field.type);
                    if (field_type == .Optional) {
                        current_field.* = try internal_receive_message(field_type.Optional.child, allocator, deserializer);
                    } else {
                        current_field.* = try internal_receive_message(field.type, allocator, deserializer);
                    }
                    present_fields[idx] = true;
                    counter += 1;
                }
            }
        }
        if (size < counter) return error.too_few_map_entries;
        var should_error = false;
        inline for (present_fields, fields) |presence, field| {
            if (!presence) {
                if (@typeInfo(field.type) == .Optional) {
                    const current_field = &@field(value, field.name);
                    current_field.* = null;
                } else {
                    std.debug.print("Missing Field in Struct {s}: {s}\n", .{ @typeName(T), field.name });
                    should_error = true;
                }
            }
        }
        if (should_error) return error.missing_field_in_struct;
    }
    return value;
}

inline fn receive_int(comptime T: type, comptime item: std.builtin.Type.Int, deserializer: Deserializer) !T {
    // TODO: eventually arbitrarily sized integers.
    var value: T = undefined;
    if (item.signedness == .signed) {
        try erlang_validate(error.decoding_signed_integer, ei.ei_decode_long(deserializer.buf.buff, deserializer.index, &value));
    } else {
        try erlang_validate(error.decoding_unsigned_integer, ei.ei_decode_ulong(deserializer.buf.buff, deserializer.index, &value));
    }
    return value;
}

inline fn receive_enum(comptime T: type, comptime item: std.builtin.Type.Enum, allocator: std.mem.Allocator, deserializer: Deserializer) !T {
    const name = try receive_atom(deserializer, allocator);
    inline for (item.fields) |field| {
        if (std.mem.eql(u8, field.name, name)) {
            return std.meta.stringToEnum(T, name) orelse error.invalid_tag_to_enum;
        }
    }
    return error.could_not_decode_enum;
}

inline fn receive_union(comptime T: type, comptime item: std.builtin.Type.Union, allocator: std.mem.Allocator, deserializer: Deserializer) !T {
    var value: T = undefined;
    var arity: i32 = 0;
    var typ: i32 = 0;
    var _v: i32 = undefined;
    try erlang_validate(
        error.decoding_get_type,
        ei.ei_get_type(deserializer.buf.buff, deserializer.index, &typ, &_v),
    );
    if (typ == ei.ERL_ATOM_EXT) {
        const enum_type = std.meta.Tag(T);
        const tuple_name = try receive_enum(enum_type, @typeInfo(enum_type).Enum, allocator, deserializer);
        switch (tuple_name) {
            inline else => |name| {
                inline for (item.fields) |field| {
                    if (field.type == void and comptime std.mem.eql(
                        u8,
                        field.name,
                        @tagName(name),
                    )) {
                        value = name;
                        break;
                    }
                }
            },
        }
        return value;
    } else {
        try erlang_validate(
            error.decoding_tuple,
            ei.ei_decode_tuple_header(deserializer.buf.buff, deserializer.index, &arity),
        );
        if (arity != 2) {
            return error.wrong_arity_for_tuple;
        }
        const tuple_name = try receive_atom(deserializer, allocator);
        inline for (item.fields) |field| {
            if (std.mem.eql(u8, field.name, tuple_name)) {
                const tuple_value = try internal_receive_message(field.type, allocator, deserializer);
                value = @unionInit(T, field.name, tuple_value);
                return value;
            }
        }
    }
    return error.unknown_tuple_tag;
}

inline fn receive_pointer(comptime T: type, comptime item: std.builtin.Type.Pointer, allocator: std.mem.Allocator, deserializer: Deserializer) !T {
    var value: T = undefined;
    if (item.size != .Slice)
        return error.unsupported_pointer_type;
    var size: i32 = 0;
    try erlang_validate(
        error.decoding_list_in_pointer_1,
        ei.ei_decode_list_header(deserializer.buf.buff, deserializer.index, &size),
    );
    const has_sentinel = item.sentinel != null;
    if (size == 0 and !has_sentinel) {
        value = &.{};
    } else {
        const usize_size: u32 = @bitCast(size);
        const slice_buffer = if (has_sentinel)
            try allocator.allocSentinel(
                item.child,
                usize_size,
                item.sentinel.?,
            )
        else
            try allocator.alloc(
                item.child,
                usize_size,
            );
        errdefer allocator.free(slice_buffer);
        for (slice_buffer) |*elem| {
            elem.* = try internal_receive_message(item.child, allocator, deserializer);
        }
        try erlang_validate(
            error.decoding_list_in_pointer_2,
            ei.ei_decode_list_header(deserializer.buf.buff, deserializer.index, &size),
        );
        if (size != 0) return error.decoded_improper_list;
        value = slice_buffer;
    }

    return value;
}

inline fn receive_array(comptime T: type, comptime item: std.builtin.Type.Array, allocator: std.mem.Allocator, deserializer: Deserializer) !T {
    var value: T = undefined;
    var size: i32 = 0;
    try erlang_validate(
        error.decoding_list_in_array_1,
        ei.ei_decode_list_header(deserializer.buf.buff, deserializer.index, &size),
    );
    if (item.len != size) return error.wrong_array_size;
    for (0..value.len) |idx| {
        value[idx] = try internal_receive_message(item.child, allocator, deserializer);
    }
    try erlang_validate(
        error.decoding_list_in_array_2,
        ei.ei_decode_list_header(deserializer.buf.buff, deserializer.index, &size),
    );
    if (size != 0) return error.decoded_improper_list;
    return value;
}

inline fn receive_bool(comptime T: type, deserializer: Deserializer) !T {
    var value: T = undefined;
    var bool_value: i32 = 0;
    try erlang_validate(
        error.decoding_boolean,
        ei.ei_decode_boolean(deserializer.buf.buff, deserializer.index, &bool_value),
    );
    if (bool_value == 0) {
        value = false;
    } else {
        value = true;
    }
    return value;
}

fn internal_receive_message(comptime T: type, allocator: std.mem.Allocator, deserializer: Deserializer) !T {
    var value: T = undefined;
    if (T == [:0]const u8) {
        value = try receive_string(deserializer, allocator);
    } else if (T == ei.erlang_pid) {
        try erlang_validate(
            error.invalid_pid,
            ei.ei_decode_pid(deserializer.buf.buff, deserializer.index, &value),
        );
    } else switch (@typeInfo(T)) {
        .Struct => |item| {
            value = try receive_struct(T, item, allocator, deserializer);
        },
        .Int => |item| {
            value = try receive_int(T, item, deserializer);
        },
        .Enum => |item| {
            value = try receive_enum(T, item, allocator, deserializer);
        },
        .Union => |item| {
            value = try receive_union(T, item, allocator, deserializer);
        },
        .Pointer => |item| {
            value = try receive_pointer(T, item, allocator, deserializer);
        },
        .Array => |item| {
            value = try receive_array(T, item, allocator, deserializer);
        },
        .Bool => {
            value = try receive_bool(T, deserializer);
        },
        .Void => {
            @compileError("Void is not supported for deserialization");
        },
        else => {
            @compileError("Unsupported type in deserialization");
        },
    }
    return value;
}
