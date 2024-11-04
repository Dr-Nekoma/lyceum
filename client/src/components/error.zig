const config = @import("../config.zig");
const rl = @import("raylib");
const std = @import("std");

pub const defaultErrorDuration = 3;
const padding = 2;
const fontSize = 22;

const Error = @Type(.{ .Enum = .{
    .is_exhaustive = true,
    .decls = &.{},
    .tag_type = blk: {
        const possible_messages = @typeInfo(errorMessage).Struct.decls.len;
        const bits = std.math.log2(possible_messages);
        break :blk std.meta.Int(
            .unsigned,
            bits + @intFromBool(@popCount(possible_messages) != 1),
        );
    },
    .fields = blk: {
        const field_names = @typeInfo(errorMessage).Struct.decls;
        var decls: [field_names.len]std.builtin.Type.EnumField = undefined;
        for (field_names, 0..) |fieldDecl, index| {
            decls[index] = .{ .value = index, .name = fieldDecl.name };
        }
        break :blk &decls;
    },
} });

const errorMessage = struct {
    pub const node_status = "Failed to Initialize Node!";
    pub const node_connection = "Node Connection Failure!";
    pub const login_send = "Failed to Send Credentials!";
    pub const login_receive = "Server did not Respond for Login";
    pub const login_invalid = "Invalid Credentials!";
    pub const get_characters_send = "Failed to Request Characters!";
    pub const get_characters_receive = "Server did not Respond with Characters";
    pub const get_characters_invalid = "Invalid Characters to Display!";
    pub const joining_map_send = "Failed asking to join Map of Selected Character";
    pub const joining_map_receive = "Could not to join Map of Selected Character";
    pub const loading_assets = "Failed loading assets of Character";
    pub const exit_send = "Failed asking to exit the Map";
    pub const exit_receive = "Could not exit the Map";
    pub const update_character_send = "Failed updating Character";
    pub const update_character_receive = "Could not ping the Server";
    pub const logout_send = "Failed asking to Logout";
    pub const logout_receive = "Server did not Respond for Logout";
    pub const logout_invalid = "Could not Logout";
};

expiration: f64 = 0,
transparency: u8 = 255,
type: ?Error = null,

pub fn update(self: *@This(), tag: Error) void {
    self.expiration = rl.getTime() + defaultErrorDuration;
    self.type = tag;
    self.transparency = 255;
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
    font: *rl.Font,
) void {
    const current_timestamp = rl.getTime();
    if (current_timestamp <= self.expiration) {
        if (self.type) |kind| {
            const message = error_message(kind);
            const textSize: f32 = rl.measureTextEx(font.*, message, fontSize, config.textSpacing).x;
            const size: rl.Vector2 = .{
                .x = 7 * padding + textSize,
                .y = 7 * padding + fontSize,
            };

            const position: rl.Vector2 = .{
                .x = width / 2 - size.x / 2,
                .y = height / 3 - size.y / 2,
            };

            const black = rl.Color.init(0, 0, 0, self.transparency);
            const red = rl.Color.init(255, 0, 0, self.transparency);

            rl.drawRectangleV(position, size, black);
            rl.drawRectangleLines(
                @intFromFloat(position.x),
                @intFromFloat(position.y),
                @intFromFloat(size.x),
                @intFromFloat(size.y),
                red,
            );

            const messageX = position.x + size.x / 2 - textSize / 2;
            const floatFont: f32 = @floatFromInt(fontSize);
            const messageY = position.y + size.y / 2 - floatFont / 2;
            rl.drawTextEx(
                font.*,
                message,
                .{ .x = messageX, .y = messageY },
                fontSize,
                config.textSpacing,
                red,
            );

            self.transparency -= 1;
        }
    }
}
