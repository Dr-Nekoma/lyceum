pub const ei = @cImport({
    @cInclude("ei.h");
});
const erl = @import("config.zig");

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

fn fancy_send(buf: *ei.ei_x_buff, data: anytype) !void {
    const Data = @TypeOf(data);

    return if (Data == *const ei.erlang_pid or Data == *ei.erlang_pid)
        erl.validate(
            error.encode_pid,
            ei.ei_x_encode_pid(buf, data),
        )
    else if (Data == ei.erlang_pid)
        fancy_send(buf, &data)
    else if (Data == []const u8 or
        Data == [:0]const u8 or
        Data == []u8 or
        Data == [:0]u8)
        erl.validate(
            error.encode_binary,
            // I think we should lean towards binaries over strings
            ei.ei_x_encode_binary_len(buf, data.ptr, data.len),
        )
    else switch (@typeInfo(Data)) {
        .Bool => erl.validate(
            error.encode_bool,
            ei.ei_x_encode_boolean(buf, @intFromBool(data)),
        ),
        .ComptimeInt => fancy_send(
            buf,
            // not sure if this conditional actually compiles
            @as(if (0 <= data) u64 else i64, data),
        ),
        .ComptimeFloat => fancy_send(buf, @as(f64, data)),
        .Int => |info| if (65 <= info.bits)
            @compileError("unsupported integer size")
        else if (info.signedness == .signed)
            erl.validate(
                error.encode_int,
                ei.ei_x_encode_longlong(buf, data),
            )
        else
            erl.validate(
                error.encode_uint,
                ei.ei_x_encode_ulonglong(buf, data),
            ),

        .Float => |info| if (65 <= info.bits)
            @compileError("unsupported float size")
        else
            erl.validate(
                error.encode_float,
                ei.ei_x_encode_double(buf, data),
            ),
        .Enum, .EnumLiteral => blk: {
            const name = @tagName(data);
            break :blk erl.validate(
                error.encode_atom,
                ei.ei_x_encode_atom_len(buf, name.ptr, name.len),
            );
        },
        .Array, .Struct, .Union => fancy_send(buf, &data),
        .Pointer => unreachable, // TODO
        .NoReturn => unreachable, // there are no actual values of this type
        else => @compileError("unsupported type"),
    };
}

// TODO: Use metaprogramming to use this as intermediate or not even
// It will be symetrical with receiver
fn send_erlang_data(buf: *ei.ei_x_buff, data: Erlang_Data) !void {
    switch (data) {
        .atom => |item| {
            try erl.validate(error.encode_atom, ei.ei_x_encode_atom(buf, item.ptr));
        },
        .tuple => |items| {
            try erl.validate(error.encode_tuple_header, ei.ei_x_encode_tuple_header(buf, @bitCast(items.len)));
            for (items) |elem| {
                try send_erlang_data(buf, elem);
            }
        },
        .list => |items| {
            try erl.validate(error.encode_list_header, ei.ei_x_encode_list_header(buf, @bitCast(items.len)));
            for (items) |elem| {
                try send_erlang_data(buf, elem);
            }
        },
        .pid => |pid| {
            try erl.validate(error.encode_pid, ei.ei_x_encode_pid(buf, pid));
        },
        .map => |entries| {
            try erl.validate(error.encode_map_header, ei.ei_x_encode_map_header(buf, @bitCast(entries.len)));
            for (entries) |entry| {
                for (entry) |value| {
                    try send_erlang_data(buf, value);
                }
            }
        },
        .string => |str| {
            try erl.validate(error.encode_string, ei.ei_x_encode_string(buf, str.ptr));
        },
        .number => |number| {
            switch (number) {
                inline else => |value| {
                    const number_type = @typeInfo(@TypeOf(value)).Int;
                    if (number_type.signedness == .signed) {
                        try erl.validate(error.encode_number, ei.ei_x_encode_long(buf, @intCast(value)));
                    } else {
                        try erl.validate(error.encode_number, ei.ei_x_encode_ulong(buf, @intCast(value)));
                    }
                },
            }
        },
    }
}

pub fn run(ec: *erl.Node, data: anytype) !void {
    var buf: ei.ei_x_buff = undefined;
    try erl.validate(error.new_with_version, ei.ei_x_new_with_version(&buf));
    try fancy_send(&buf, data);
    try erl.validate(error.reg_send_failed, ei.ei_reg_send(&ec.c_node, ec.fd, @constCast(erl.process_name), buf.buff, buf.index));
}

pub fn run_with_self(ec: *erl.Node, data: Erlang_Data) !void {
    return run(ec, .{ .tuple = &.{ .{ .pid = ei.ei_self(&ec.c_node) }, data } });
}

pub fn run_string(ec: *erl.Node, message: [:0]const u8) !void {
    return run_with_self(ec, .{ .atom = message });
}
