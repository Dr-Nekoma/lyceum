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

pub const MapExample = struct {
    x: i32,
    y: i32,
}

pub const AtomExample = enum {
    something,
    anything
}

pub const TupleExample = union(enum) {
    something: AtomExample,
    anything: AtomExample
}

pub const Mock = struct {
    a: i32; // Range check
    b: [:0]const u8;
    c: MapExample,
    d: []const u8,
    e: AtomExample,
    f: TupleExample,
}

pub fn receive_string(buf: *ei.ei_x_buff, index: *i32, allocator: std.mem.Allocator) ![:0]const u8 {
    var string_length: i32 = undefined;
    var ty: i32 = undefined;
    try erlang_validate(error.deconding_string_length, ei.ei_get_type(buf.buff, &index, &ty, &string_length));

    if (ty != ei.ERL_STRING_EXT)
        return error.message_is_not_string;

    const ustring_length: u32 = @bitCast(string_length);

    const allocator = std.heap.c_allocator;
    const string_buffer = try allocator.alloc(u8, ustring_length);
    try erlang_validate(error.decoding_string, ei.ei_decode_string(buf.buff, &index, string_buffer.ptr));
    return string_buffer;
}

// Here is a sketch of an idea
// .{ .map = .{ .size = 2,
//              .keys = ["this", "that"]}
// TODO: Try to make a nice system just like for sending
// TODO: Do proper error handling in receive_message
pub fn receive_message(allocator: std.mem.Allocator, comptime T: type) !T {
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
    
    var value: T = undefined;
    switch (@typeInfo(T)) {
        .Struct => {
            inline for (meta.fields(T)) |struct_field| {
                switch (@typeInfo(struct_field.type)) {
                    .Int => {
                        if (struct_field.default_value != null) {
                            var anyopaque_pointer: *anyopaque = @constCast(struct_field.default_value.?);
                            var x = @ptrCast(*const u32, @alignCast(4, anyopaque_pointer));
                            struct_field.name = x;
                            std.debug.print("default value {any}\n", .{x});
                        }
                    },
                    else => @compileError("TODO!!\n"),
                }
                try deserializeInto(&@field(value, struct_field.name));
            }
            return value;
        },
        .Int => |item| {
            // TODO: eventually arbitrarily sized integers.
            if (item.signedness == .signed) {
                try erlang_validate(error.decoding_signed_integer, ei.decode_long(buf.buff, &index, &value));
            } else {
                try erlang_validate(error.decoding_unsigned_integer, ei.decode_ulong(buf.buff, &index, &value));
            }
            return value;
        },
        .Enum => |item| {
            try erlang_validate(error.decoding_atom, ei.decode_atom(buf.buff, &index, &value));
            for (item.fields) |field| {
                if (std.mem.eql(u8, field.name, value)) {
                    return std.meta.stringToEnum(T, value);
                }
            }
            return error.could_not_decode_enum;
        },
        .Union => |item| {
            var arity: i32 = 0;
            try erlang_validate(error.decoding_tuple, ei.ei_decode_tuple_header(buf.buff, &index, &arity));
            if (arity != 2) {
                return error.wrong_arity_for_tuple;
            }

            const tuple_name = try receive_string(&buf, &index, allocator);
            const tuple_value = try receive_message(allocator, T);
           
        }
    }    
}

pub fn receive_message(ec: *LNode) ![]u8 {
    var msg: ei.erlang_msg = undefined;
    var index: i32 = 0;
    var version: i32 = undefined;
    var arity: i32 = 0;
    var pid: ei.erlang_pid = undefined;
    
    _ = ei.ei_decode_version(buf.buff, &index, &version);
    _ = ei.ei_decode_tuple_header(buf.buff, &index, &arity);
    if (arity != 2) {
        return error.got_wrong_message;
    }
    _ = ei.ei_decode_pid(buf.buff, &index, &pid);

    _ = ei.ei_decode_string(buf.buff, &index, string_buffer.ptr);

    return string_buffer;
}
