const config = @import("../config.zig");
const rl = @import("raylib");
const std = @import("std");

pub const defaultErrorDuration = 3;
const padding = 2;
const fontSize = 22;
const errorBackgroundColor = rl.Color.init(0, 0, 0, 255);

const Error = @Type(.{ .Enum = .{
    .is_exhaustive = true,
    .decls = &.{},
    .tag_type = u16,
    .fields = blk: {
        const field_names = @typeInfo(errorMessage).Struct.decls;
        var decls: [field_names.len]std.builtin.Type.EnumField = undefined;
        for (field_names, 0..) |fieldDecl, index| {
            decls[index] = .{ .value = index, .name = fieldDecl.name };
        }
        break :blk &decls;
    },
} });

pub const errorMessage = struct {
    pub const node_status = "Failed to Initialize Node!";
    pub const node_connection = "Node Connection Failure!";
    pub const login_send = "Failed to Send Credentials!";
    pub const login_receive = "Could not Login";
    pub const joining_map_send = "Failed asking to join Map of Selected Character";
    pub const joining_map_receive = "Could not to join Map of Selected Character";
    pub const loading_assets = "Failed loading assets of Character";
    pub const exit_send = "Failed asking to exit the Map";
    pub const exit_receive = "Could not exit the Map";
    pub const update_character_send = "Failed updating Character";
    pub const update_character_receive = "Could not ping the Server";
    pub const logout_send = "Failed asking to Logout";
    pub const logout_receive = "Could not Logout";
};

expiration: f64 = 0,
type: ?Error = null,

pub fn update(self: *@This(), tag: Error) void {
    self.expiration = rl.getTime() + defaultErrorDuration;
    self.type = tag;
}

fn error_message(kind: Error) [:0]const u8 {
    return switch (kind) {
        inline else => |tag| @field(errorMessage, @tagName(tag)),
    };
}

pub fn at(
    self: *@This(),
    width: f32,
    height: f32,
) void {
    const current_timestamp = rl.getTime();
    if (current_timestamp <= self.expiration) {
        if (self.type) |kind| {
            const message = error_message(kind);
            const textSize: f32 = @floatFromInt(rl.measureText(message, fontSize));
            const size: rl.Vector2 = .{
                .x = 7 * padding + textSize,
                .y = 7 * padding + fontSize,
            };

            const position: rl.Vector2 = .{
                .x = width / 2 - size.x / 2,
                .y = height / 4 - size.y / 2,
            };

            rl.drawRectangleV(position, size, errorBackgroundColor);
            rl.drawRectangleLines(
                @intFromFloat(position.x),
                @intFromFloat(position.y),
                @intFromFloat(size.x),
                @intFromFloat(size.y),
                rl.Color.red,
            );

            const messageX = position.x + size.x / 2 - textSize / 2;
            const floatFont: f32 = @floatFromInt(fontSize);
            const messageY = position.y + size.y / 2 - floatFont / 2;
            rl.drawText(
                message,
                @intFromFloat(messageX),
                @intFromFloat(messageY),
                fontSize,
                rl.Color.red,
            );
        }
    }
}
