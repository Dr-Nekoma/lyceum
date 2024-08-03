pub const ei = @cImport({
    @cInclude("ei.h");
});

pub const std = @import("std");

// TODO: move these elsewhere, maybe make them into parameters
pub const process_name = "lyceum_server";
pub const server_name = process_name ++ "@localhost";

pub const Send_Error = error{
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

fn send_pointer(buf: *ei.ei_x_buff, data: anytype) Send_Error!void {
    const Data = @TypeOf(data);
    const info = @typeInfo(Data).Pointer;
    switch (info.size) {
        .Many, .C => @compileError("unsupported pointer size"),
        .Slice => {
            try validate(
                error.could_not_encode_list_head,
                ei.ei_x_encode_list_header(buf, @bitCast(data.len)),
            );
            for (data) |item| try send_payload(buf, item);
            try validate(
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
                .Array => |array_info| try send_payload(
                    buf,
                    @as([]const array_info.child, data),
                ),
                .Union => |union_info| switch (@as(
                    if (union_info.tag_type) |tag_type|
                        tag_type
                    else
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
                                    try validate(
                                        error.could_not_encode_tuple,
                                        ei.ei_x_encode_tuple_header(buf, 2),
                                    );
                                }
                                try send_payload(buf, tag);
                                if (send_tuple) {
                                    try send_payload(buf, @field(data, @tagName(tag)));
                                }
                            }
                        }
                    },
                },
                .Struct => |struct_info| if (struct_info.is_tuple) {
                    try validate(
                        error.could_not_encode_tuple,
                        ei.ei_x_encode_tuple_header(buf, struct_info.fields.len),
                    );
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
                    try validate(
                        error.could_not_encode_map,
                        ei.ei_x_encode_map_header(buf, @bitCast(present_fields)),
                    );
                    inline for (struct_info.fields) |field| {
                        const payload = @field(data, field.name);
                        if (@typeInfo(@TypeOf(payload)) != .Optional or
                            payload != null)
                        {
                            try validate(
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

fn send_payload(buf: *ei.ei_x_buff, data: anytype) Send_Error!void {
    const Data = @TypeOf(data);

    return if (Data == *const ei.erlang_pid or
        Data == *ei.erlang_pid or
        Data == [*c]const ei.erlang_pid or
        Data == [*c]ei.erlang_pid)
        validate(
            error.could_not_encode_pid,
            ei.ei_x_encode_pid(buf, data),
        )
    else if (Data == ei.erlang_pid)
        send_payload(buf, &data)
    else if (Data == []const u8 or
        Data == [:0]const u8 or
        Data == []u8 or
        Data == [:0]u8)
        validate(
            error.could_not_encode_binary,
            // I think we should lean towards binaries over strings
            ei.ei_x_encode_binary(buf, data.ptr, @intCast(data.len)),
        )
    else switch (@typeInfo(Data)) {
        .Bool => validate(
            error.could_not_encode_bool,
            ei.ei_x_encode_boolean(buf, @intFromBool(data)),
        ),
        .ComptimeInt => send_payload(
            buf,
            // not sure if this conditional actually compiles
            @as(if (0 <= data) u64 else i64, data),
        ),
        .ComptimeFloat => send_payload(buf, @as(f64, data)),
        .Int => |info| if (65 <= info.bits)
            @compileError("unsupported integer size")
        else if (info.signedness == .signed)
            validate(
                error.could_not_encode_int,
                ei.ei_x_encode_longlong(buf, data),
            )
        else
            validate(
                error.could_not_encode_uint,
                ei.ei_x_encode_ulonglong(buf, data),
            ),

        .Float => |info| if (65 <= info.bits)
            @compileError("unsupported float size")
        else
            validate(
                error.could_not_encode_float,
                ei.ei_x_encode_double(buf, data),
            ),
        .Enum, .EnumLiteral => blk: {
            const name = @tagName(data);
            break :blk validate(
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

pub const Node = struct {
    c_node: ei.ei_cnode,
    fd: i32,
    node_name: [:0]const u8 = "lyceum_client",
    cookie: [:0]const u8 = "lyceum",

    pub fn send(ec: *Node, data: anytype) !void {
        var buf: ei.ei_x_buff = undefined;
        // TODO: get rid of hidden allocation
        try validate(error.new_with_version, ei.ei_x_new_with_version(&buf));
        defer _ = ei.ei_x_free(&buf);

        try send_payload(&buf, data);
        try validate(
            error.reg_send_failed,
            ei.ei_reg_send(&ec.c_node, ec.fd, @constCast(process_name), buf.buff, buf.index),
        );
    }

    pub fn self(ec: *Node) !*ei.erlang_pid {
        return if (ei.ei_self(&ec.c_node)) |pid|
            pid
        else
            error.could_not_recover_self_pid;
    }
};

pub fn validate(error_tag: anytype, result_value: c_int) !void {
    if (result_value < 0) {
        return error_tag;
    }
}

pub fn establish_connection(ec: *Node) !void {
    const sockfd = ei.ei_connect(&ec.c_node, @constCast(server_name));
    try validate(error.ei_connect_failed, sockfd);
    ec.fd = sockfd;
}

pub fn prepare_connection() !Node {
    var l_node: Node = .{
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
