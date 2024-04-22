const ei = @cImport({
    @cInclude("ei.h");
});

const std = @import("std");
const server_name = "lyceum_server@nixos";

const LNode = struct {
    c_node: ei.ei_cnode,
    fd: i32,
    node_name: [:0]const u8 = "lyceum_client@nixos",
    port: i32 = 4369,
    identification_number: i32 = 99,
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
    const sockfd: i32 = ei.ei_connect_host_port(&ec.c_node, @constCast(server_name), ec.port);
    if (sockfd < 0) return error.ei_connect_failed;
    ec.fd = sockfd;
}

pub fn send_message(ec: *LNode, message: [:0]const u8) void {
    var buf: ei.ei_x_buff = undefined;
    _ = ei.ei_x_new_with_version(&buf);
    _ = ei.ei_x_encode_tuple_header(&buf, 2);
    _ = ei.ei_x_encode_pid(&buf, ei.ei_self(&ec.c_node));
    _ = ei.ei_x_encode_atom(&buf, message.ptr);
    _ = ei.ei_reg_send(&ec.c_node, ec.fd, @constCast(server_name), buf.buff, buf.index);
}

pub fn main() !void {
    const connection_status = ei.ei_init();
    if (connection_status != 0) return error.ei_init_failed;
    var node: LNode = try prepare_connection();
    try establish_connection(&node);
    send_message(&node, "Hello from Lyceum!");
}
