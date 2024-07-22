pub const ei = @cImport({
    @cInclude("ei.h");
});
const erl = @import("config.zig");
const std = @import("std");

pub const Number = union(enum) {
    eu8: u8,
    eu16: u16,
    eu32: u32,
    eu64: u64,
    ei8: i8,
    ei16: i16,
    ei32: i32,
    ei64: i64,
};

pub const Erlang_Data = union(enum) {
    atom: [:0]const u8,
    tuple: []const Erlang_Data,
    pid: *const ei.erlang_pid,
    map: []const [2]Erlang_Data,
    string: [:0]const u8,
    list: []const Erlang_Data,
    number: Number,
};

inline fn send_pointer(buf: *ei.ei_x_buff, data: anytype) !void {
    const Data = @TypeOf(data);
    const info = switch (@typeInfo(Data)) {
        .Pointer => |info| info,
        else => @compileError("not a pointer type"),
    };
    switch (info.size) {
        .Many, .C => @compileError("unsupported pointer size"),
        .Slice => {
            try erl.validate(
                error.could_not_encode_list_header,
                ei.ei_x_encode_list_header(buf, @bitCast(data.len)),
            );
            for (data) |item| try send(buf, item);
            try erl.validate(
                error.could_not_encode_list_tail,
                ei.ei_x_encode_list_header(buf, 0),
            );
        },
        .One => {
            // Fixme
            const Child = info.child;
            switch (@typeInfo(Child)) {
                .Bool,
                .Int,
                .Float,
                .Enum,
                .Pointer,
                // not sure if these are actually reachable
                .EnumLiteral,
                .ComptimeInt,
                .ComptimeFloat,
                => try send(buf, data.*),
                .Array => |array_info| try send(
                    buf,
                    @as([]const array_info.child, data),
                ),
                .Union => |union_info| switch (@as(union_info.tag_type, data)) {
                    inline else => |tag| {
                        inline for (union_info.fields) |field| {
                            if (comptime std.mem.eql(
                                u8,
                                field.name,
                                @tagName(tag),
                            )) {
                                const send_tuple: bool = field.type != void;
                                if (send_tuple) {
                                    try erl.validate(
                                        error.could_not_encode_tuple,
                                        ei.ei_x_encode_tuple_header(buf, 2),
                                    );
                                }
                                try send(buf, tag);
                                if (send_tuple) {
                                    try send(buf, switch (data) {
                                        tag => |payload| payload,
                                        else => unreachable,
                                    });
                                }
                            }
                        }
                    },
                },
                .Struct => unreachable, // TODO
                .NoReturn => unreachable,
                else => @compileError("unsupported type"),
            }
        },
    }
}

pub fn send(buf: *ei.ei_x_buff, data: anytype) !void {
    const Data = @TypeOf(data);

    return if (Data == *const ei.erlang_pid or Data == *ei.erlang_pid)
        erl.validate(
            error.could_not_encode_pid,
            ei.ei_x_encode_pid(buf, data),
        )
    else if (Data == ei.erlang_pid)
        send(buf, &data)
    else if (Data == []const u8 or
        Data == [:0]const u8 or
        Data == []u8 or
        Data == [:0]u8)
        erl.validate(
            error.could_not_encode_binary,
            // I think we should lean towards binaries over strings
            ei.ei_x_encode_binary_len(buf, data.ptr, data.len),
        )
    else switch (@typeInfo(Data)) {
        .Bool => erl.validate(
            error.could_not_encode_bool,
            ei.ei_x_encode_boolean(buf, @intFromBool(data)),
        ),
        .ComptimeInt => send(
            buf,
            // not sure if this conditional actually compiles
            @as(if (0 <= data) u64 else i64, data),
        ),
        .ComptimeFloat => send(buf, @as(f64, data)),
        .Int => |info| if (65 <= info.bits)
            @compileError("unsupported integer size")
        else if (info.signedness == .signed)
            erl.validate(
                error.could_not_encode_int,
                ei.ei_x_encode_longlong(buf, data),
            )
        else
            erl.validate(
                error.could_not_encode_uint,
                ei.ei_x_encode_ulonglong(buf, data),
            ),

        .Float => |info| if (65 <= info.bits)
            @compileError("unsupported float size")
        else
            erl.validate(
                error.could_not_encode_float,
                ei.ei_x_encode_double(buf, data),
            ),
        .Enum, .EnumLiteral => blk: {
            const name = @tagName(data);
            break :blk erl.validate(
                error.could_not_encode_atom,
                ei.ei_x_encode_atom_len(buf, name.ptr, name.len),
            );
        },
        .Array, .Struct, .Union => send(buf, &data),
        .Pointer => send_pointer(buf, data),
        .NoReturn => unreachable,
        else => @compileError("unsupported type"),
    };
}

// FIXME: rewrite the things below this to make sense
pub fn run(ec: *erl.Node, data: anytype) !void {
    var buf: ei.ei_x_buff = undefined;
    try erl.validate(error.new_with_version, ei.ei_x_new_with_version(&buf));
    try send(&buf, data);
    try erl.validate(error.reg_send_failed, ei.ei_reg_send(&ec.c_node, ec.fd, @constCast(erl.process_name), buf.buff, buf.index));
}

pub fn run_with_self(ec: *erl.Node, data: Erlang_Data) !void {
    return run(ec, .{ .tuple = &.{ .{ .pid = ei.ei_self(&ec.c_node) }, data } });
}

pub fn run_string(ec: *erl.Node, message: [:0]const u8) !void {
    return run_with_self(ec, .{ .atom = message });
}
