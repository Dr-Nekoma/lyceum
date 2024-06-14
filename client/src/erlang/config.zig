pub const ei = @cImport({
    @cInclude("ei.h");
});

pub const std = @import("std");
pub const process_name = "lyceum_server";
pub const server_name = process_name ++ "@nixos";

pub const Node = struct {
    c_node: ei.ei_cnode,
    fd: i32,
    node_name: [:0]const u8 = "lyceum_client",
    cookie: [:0]const u8 = "lyceum",
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
