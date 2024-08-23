pub const ei = @cImport({
    @cInclude("ei.h");
});
const erl = @import("zerl");
const std = @import("std");

pub const Error = error{
    could_not_encode_pid,
    could_not_encode_binary,
    could_not_encode_map,
    could_not_encode_atom,
    could_not_encode_tuple,
    could_not_encode_int,
    could_not_encode_uint,
    could_not_encode_list_head,
    could_not_encode_list_tail,
};

fn send_pointer(buf: *ei.ei_x_buff, data: anytype) Error!void {
    const Data = @TypeOf(data);
    const info = @typeInfo(Data).Pointer;
    switch (info.size) {
        .Many, .C => @compileError("unsupported pointer size"),
        .Slice => {
            try erl.validate(
                error.could_not_encode_list_head,
                ei.ei_x_encode_list_header(buf, @bitCast(data.len)),
            );
            for (data) |item| try send_payload(buf, item);
            try erl.validate(
                error.could_not_encode_list_tail,
                ei.ei_x_encode_list_header(buf, 0),
            );
        },
        .One => {
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
                => try send_payload(buf, data.*),
                .Array => |array_info| try send_pointer(
                    buf,
                    @as([]const array_info.child, data),
                ),
                .Union => |union_info| switch (@as(
                    if (union_info.tag_type) |tag_type|
                        tag_type
                    else
                        // TODO: consider sending untagged unions
                        @compileError("untagged unions are unsupported"),
                    data.*,
                )) {
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
                                try send_payload(buf, tag);
                                if (send_tuple) try send_payload(buf, @field(data, @tagName(tag)));
                            }
                        }
                    },
                },
                .Struct => |struct_info| if (struct_info.is_tuple) {
                    try erl.validate(
                        error.could_not_encode_tuple,
                        ei.ei_x_encode_tuple_header(buf, struct_info.fields.len),
                    );
                    // TODO: check if we can use an inline for instead
                    comptime var i = 0;
                    inline while (i < struct_info.fields.len) : (i += 1) {
                        try send_payload(buf, @field(
                            data,
                            std.fmt.comptimePrint("{}", .{i}),
                        ));
                    }
                } else {
                    const mandatory_fields = comptime blk: {
                        var count = 0;
                        for (struct_info.fields) |field| {
                            const field_info = @typeInfo(field.type);
                            if (field_info != .Optional) count += 1;
                        }
                        break :blk count;
                    };
                    var present_fields: usize = mandatory_fields;
                    inline for (struct_info.fields) |field| {
                        const field_info = @typeInfo(field.type);
                        const payload = @field(data, field.name);
                        if (field_info == .Optional) {
                            if (payload != null) {
                                present_fields += 1;
                            }
                        }
                    }
                    try erl.validate(
                        error.could_not_encode_map,
                        ei.ei_x_encode_map_header(buf, @bitCast(present_fields)),
                    );
                    inline for (struct_info.fields) |field| {
                        const payload = @field(data, field.name);
                        if (@typeInfo(@TypeOf(payload)) != .Optional or
                            payload != null)
                        {
                            try erl.validate(
                                error.could_not_encode_atom,
                                ei.ei_x_encode_atom_len(
                                    buf,
                                    field.name.ptr,
                                    @intCast(field.name.len),
                                ),
                            );
                            try send_payload(buf, payload);
                        }
                    }
                },
                .NoReturn => unreachable,
                else => @compileError("unsupported type"),
            }
        },
    }
}

pub fn send_payload(buf: *ei.ei_x_buff, data: anytype) Error!void {
    const Data = @TypeOf(data);

    return if (Data == *const ei.erlang_pid or
        Data == *ei.erlang_pid or
        Data == [*c]const ei.erlang_pid or
        Data == [*c]ei.erlang_pid)
        erl.validate(
            error.could_not_encode_pid,
            ei.ei_x_encode_pid(buf, data),
        )
    else if (Data == ei.erlang_pid)
        send_payload(buf, &data)
    else if (Data == []const u8 or
        Data == [:0]const u8 or
        Data == []u8 or
        Data == [:0]u8)
        erl.validate(
            error.could_not_encode_binary,
            // I think we should lean towards binaries over strings
            // TODO: make that happen
            ei.ei_x_encode_string_len(buf, data.ptr, @intCast(data.len)),
        )
    else switch (@typeInfo(Data)) {
        .Bool => erl.validate(
            error.could_not_encode_bool,
            ei.ei_x_encode_boolean(buf, @intFromBool(data)),
        ),
        // TODO: make inline fn to handle this properly
        .ComptimeInt => send_payload(
            buf,
            // not sure if this conditional actually compiles
            @as(if (0 <= data) u64 else i64, data),
        ),
        .ComptimeFloat => send_payload(buf, @as(f64, data)),
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
                ei.ei_x_encode_atom_len(buf, name.ptr, @intCast(name.len)),
            );
        },
        .Array, .Struct, .Union => send_payload(buf, &data),
        .Pointer => send_pointer(buf, data),
        .NoReturn => unreachable,
        else => @compileError("unsupported type"),
    };
}
