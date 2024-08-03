pub const ei = @cImport({
    @cInclude("ei.h");
});

const std = @import("std");
const receiver = @import("receiver.zig");
const sender = @import("sender.zig");

// TODO: move these elsewhere, maybe make them into parameters
pub const process_name = "lyceum_server";
pub const server_name = process_name ++ "@localhost";

pub const Send_Error = sender.Error || error{
    // TODO: rid the world of these terrible names
    new_with_version,
    reg_send_failed,
};

pub const Node = struct {
    c_node: ei.ei_cnode,
    fd: i32,
    node_name: [:0]const u8 = "lyceum_client",
    cookie: [:0]const u8 = "lyceum",

    pub fn receive(ec: *Node, comptime T: type, allocator: std.mem.Allocator) !T {
        return receiver.run(T, allocator, ec);
    }

    pub fn send(ec: *Node, data: anytype) Send_Error!void {
        var buf: ei.ei_x_buff = undefined;
        // TODO: get rid of hidden allocation
        try validate(error.new_with_version, ei.ei_x_new_with_version(&buf));
        defer _ = ei.ei_x_free(&buf);

        try sender.send_payload(&buf, data);
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

pub fn With_Pid(comptime T: type) type {
    return std.meta.Tuple(&.{ ei.erlang_pid, T });
}
