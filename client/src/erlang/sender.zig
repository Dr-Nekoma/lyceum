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

pub fn run(ec: *erl.Node, data: Erlang_Data) !void {
    var buf: ei.ei_x_buff = undefined;
    try erl.validate(error.new_with_version, ei.ei_x_new_with_version(&buf));
    try send_erlang_data(&buf, data);
    if (ec.handler) |*pid| {
        try erl.validate(error.send_failed, ei.ei_send(ec.fd, pid, buf.buff, buf.index));
    } else {
        try erl.validate(error.reg_send_failed, ei.ei_reg_send(&ec.c_node, ec.fd, @constCast(erl.process_name), buf.buff, buf.index));
    }
}

pub fn run_with_self(ec: *erl.Node, data: Erlang_Data) !void {
    return run(ec, .{ .tuple = &.{ .{ .pid = ei.ei_self(&ec.c_node) }, data } });
}

pub fn run_string(ec: *erl.Node, message: [:0]const u8) !void {
    return run_with_self(ec, .{ .atom = message });
}
