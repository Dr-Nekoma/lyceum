pub const ei = @cImport({
    @cInclude("ei.h");
});
const std = @import("std");
const erl = @import("config.zig");

pub const Deserializer = struct {
    buf: *ei.ei_x_buff,
    index: *i32,
    ec: *erl.Node,
};

// TODO: Try to make the simplest possible example using something like this to report to zig repo
//pub fn receive_atom_string(deserializer: Deserializer, allocator: std.mem.Allocator, erlang_fun: fn ([*:0]const u8, *i32, [*:0]u8) callconv(.C) c_int) ![:0]const u8 {
fn receive_atom_string(deserializer: Deserializer, allocator: std.mem.Allocator, erlang_fun: fn ([*c]const u8, [*c]c_int, [*c]u8) callconv(.C) c_int) ![:0]const u8 {
    var length: i32 = undefined;
    var ty: i32 = undefined;
    try erl.validate(error.decoding_atom_string_length, ei.ei_get_type(deserializer.buf.buff, deserializer.index, &ty, &length));

    if (ty != ei.ERL_STRING_EXT and ty != ei.ERL_ATOM_EXT)
        return error.message_is_not_atom_or_string;

    const u_length: u32 = @bitCast(length);

    const buffer = try allocator.allocSentinel(u8, u_length, 0);
    try erl.validate(error.decoding_atom, erlang_fun(deserializer.buf.buff, deserializer.index, buffer.ptr));
    return buffer;
}

fn receive_string(deserializer: Deserializer, allocator: std.mem.Allocator) ![:0]const u8 {
    return receive_atom_string(deserializer, allocator, ei.ei_decode_string);
}

fn receive_atom(deserializer: Deserializer, allocator: std.mem.Allocator) ![:0]const u8 {
    return receive_atom_string(deserializer, allocator, ei.ei_decode_atom);
}

pub fn run_with_pid(comptime T: type) type {
    return std.meta.Tuple(&.{ ei.erlang_pid, T });
}

pub fn run(comptime T: type, allocator: std.mem.Allocator, ec: *erl.Node) !T {
    var msg: ei.erlang_msg = undefined;
    var buf: ei.ei_x_buff = undefined;
    var index: i32 = 0;

    try erl.validate(error.create_new_decode_buff, ei.ei_x_new(&buf));

    while (true) {
        const got: i32 = ei.ei_xreceive_msg(ec.fd, &msg, &buf);
        if (got == ei.ERL_TICK)
            continue;
        if (got == ei.ERL_ERROR) {
            return error.got_error_receiving_message;
        }
        break;
    }

    try erl.validate(error.decoding_version, ei.ei_decode_version(buf.buff, &index, null));
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
        try erl.validate(
            error.decoding_tuple,
            ei.ei_decode_tuple_header(deserializer.buf.buff, deserializer.index, &size),
        );
        if (item.fields.len != size) return error.wrong_tuple_size;
        inline for (&value) |*elem| {
            elem.* = try internal_receive_message(@TypeOf(elem.*), allocator, deserializer);
        }
    } else {
        try erl.validate(
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
        try erl.validate(error.decoding_signed_integer, ei.ei_decode_long(deserializer.buf.buff, deserializer.index, &value));
    } else {
        try erl.validate(error.decoding_unsigned_integer, ei.ei_decode_ulong(deserializer.buf.buff, deserializer.index, &value));
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
    try erl.validate(
        error.decoding_get_type,
        ei.ei_get_type(deserializer.buf.buff, deserializer.index, &typ, &_v),
    );
    const enum_type = std.meta.Tag(T);
    if (typ == ei.ERL_ATOM_EXT) {
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
                } else return error.invalid_union_tag;
            },
        }
        return value;
    } else {
        try erl.validate(
            error.decoding_tuple,
            ei.ei_decode_tuple_header(deserializer.buf.buff, deserializer.index, &arity),
        );
        if (arity != 2) {
            return error.wrong_arity_for_tuple;
        }
        const tuple_name = try receive_enum(enum_type, @typeInfo(enum_type).Enum, allocator, deserializer);
        switch (tuple_name) {
            inline else => |name| {
                inline for (item.fields) |field| {
                    if (field.type != void and comptime std.mem.eql(
                        u8,
                        field.name,
                        @tagName(name),
                    )) {
                        const tuple_value = try internal_receive_message(field.type, allocator, deserializer);
                        value = @unionInit(T, field.name, tuple_value);
                        break;
                    }
                } else return error.failed_to_receive_payload;
            },
        }
    }
    return error.unknown_tuple_tag;
}

inline fn receive_pointer(comptime T: type, comptime item: std.builtin.Type.Pointer, allocator: std.mem.Allocator, deserializer: Deserializer) !T {
    var value: T = undefined;
    if (item.size != .Slice)
        return error.unsupported_pointer_type;
    var size: i32 = 0;
    try erl.validate(
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
        try erl.validate(
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
    try erl.validate(
        error.decoding_list_in_array_1,
        ei.ei_decode_list_header(deserializer.buf.buff, deserializer.index, &size),
    );
    if (item.len != size) return error.wrong_array_size;
    for (0..value.len) |idx| {
        value[idx] = try internal_receive_message(item.child, allocator, deserializer);
    }
    try erl.validate(
        error.decoding_list_in_array_2,
        ei.ei_decode_list_header(deserializer.buf.buff, deserializer.index, &size),
    );
    if (size != 0) return error.decoded_improper_list;
    return value;
}

inline fn receive_bool(comptime T: type, deserializer: Deserializer) !T {
    var value: T = undefined;
    var bool_value: i32 = 0;
    try erl.validate(
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
        try erl.validate(
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
