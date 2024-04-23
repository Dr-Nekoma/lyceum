const ei = @cImport({
    @cInclude("ei.h");
});

const std = @import("std");
const process_name = "lyceum_server";
const server_name = process_name ++ "@nixos";

const LNode = struct {
    c_node: ei.ei_cnode,
    fd: i32,
    node_name: [:0]const u8 = "lyceum_client",
    cookie: [:0]const u8 = "lyceum",
};

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
    const sockfd: i32 = ei.ei_connect(&ec.c_node, @constCast(server_name));
    if (sockfd < 0) return error.ei_connect_failed;
    ec.fd = sockfd;
}

pub fn send_message(ec: *LNode, message: [:0]const u8) !void {
    var buf: ei.ei_x_buff = undefined;
    _ = ei.ei_x_new_with_version(&buf);
    _ = ei.ei_x_encode_tuple_header(&buf, 2);
    _ = ei.ei_x_encode_pid(&buf, ei.ei_self(&ec.c_node));
    _ = ei.ei_x_encode_atom(&buf, message.ptr);
    const result = ei.ei_reg_send(&ec.c_node, ec.fd, @constCast(process_name), buf.buff, buf.index);
    return if (result < 0)
        error.ei_reg_send_failed;
}

pub fn receive_message(ec: *LNode) ![]u8 {
    var msg: ei.erlang_msg = undefined;
    var index: i32 = 0;
    var version: i32 = undefined;
    var arity: i32 = 0;
    var pid: ei.erlang_pid = undefined;
    var buf: ei.ei_x_buff = undefined;
    _ = ei.ei_x_new(&buf);

    while (true) {
        const got: i32 = ei.ei_xreceive_msg(ec.fd, &msg, &buf);
        if (got == ei.ERL_TICK)
            continue;
        if (got == ei.ERL_ERROR) {
            return error.got_error_receiving_message;
        }
        break;
    }
    _ = ei.ei_decode_version(buf.buff, &index, &version);
    _ = ei.ei_decode_tuple_header(buf.buff, &index, &arity);
    if (arity != 2) {
        return error.got_wrong_message;
    }
    _ = ei.ei_decode_pid(buf.buff, &index, &pid);

    var string_length: i32 = undefined;
    var ty: i32 = undefined;
    _ = ei.ei_get_type(buf.buff, &index, &ty, &string_length);

    if (ty != ei.ERL_STRING_EXT)
        return error.message_is_not_string;

    const ustring_length: u32 = @bitCast(string_length);

    const allocator = std.heap.c_allocator;
    const string_buffer = try allocator.alloc(u8, ustring_length);

    _ = ei.ei_decode_string(buf.buff, &index, string_buffer.ptr);

    return string_buffer;
}

pub fn main() !void {
    const connection_status = ei.ei_init();
    if (connection_status != 0) return error.ei_init_failed;
    var node: LNode = try prepare_connection();
    try establish_connection(&node);
    try send_message(&node, "Hello from Lyceum!");
    var msg: []const u8 = try receive_message(&node);
    std.debug.print("{s}", .{msg});
}

// TODO: Do proper error handling in receive_message
// TODO: Create build.zig
// TODO: Make zig build command nix compatible
