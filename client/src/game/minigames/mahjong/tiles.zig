const config = @import("../../../config.zig");
const rl = @import("raylib");
const player = @import("player.zig");
const std = @import("std");

pub const tile_size: rl.Vector2 = .{
    .x = 60,
    .y = 100,
};

pub const Suit = enum {
    circle,
    bamboo,
    character,
    wind,
    dragon,
};

pub const Number = enum(u5) {
    one = 1,
    two = 2,
    three = 3,
    four = 4,
    five = 5,
    six = 6,
    seven = 7,
    eight = 8,
    nine = 9,
};

pub const Direction = enum {
    east,
    west,
    north,
    south,
};

pub const Color = enum { red, green, white };

pub const Tile = union(Suit) {
    circle: Number,
    bamboo: Number,
    character: Number,
    wind: Direction,
    dragon: Color,
};

pub const pile_size = blk: {
    var total = 0;
    const tile_info = @typeInfo(Tile).Union;
    for (tile_info.fields) |field| {
        total += @typeInfo(field.type).Enum.fields.len * 4;
    }
    break :blk total;
};

pub const Pile = [pile_size]Tile;

pub const drawing = struct {
    pub const Side = enum {
        left,
        right,
        top,
        down,
    };

    const initial_side_x = 150;
    const initial_top_y = 50;
    const initial_top_size_x = 60;
    const initial_top_size_y = 50;
    const initial_down_size_x = 60;
    const initial_down_size_y = 100;
    const initial_side_size_x = 40;
    const initial_side_size_y = 90;
    const initial_down_padding = 15;

    pub fn defaultSize(side: Side) rl.Vector2 {
        switch (side) {
            .left => {
                return .{
                    .x = initial_side_size_x,
                    .y = initial_side_size_y,
                };
            },
            .right => {
                return .{
                    .x = initial_side_size_x,
                    .y = initial_side_size_y,
                };
            },
            .down => {
                return .{
                    .x = initial_down_size_x,
                    .y = initial_down_size_y,
                };
            },
            .top => {
                return .{
                    .x = initial_top_size_x,
                    .y = initial_top_size_y,
                };
            },
        }
    }

    pub fn defaultPosition(width: f32, height: f32, side: Side) rl.Vector2 {
        switch (side) {
            .left => {
                return .{
                    .x = initial_side_x,
                    .y = 60,
                };
            },
            .right => {
                return .{
                    .x = width - initial_side_x - initial_side_size_x,
                    .y = 60,
                };
            },
            .down => {
                return .{
                    .x = width / 2 - (initial_down_size_x + initial_down_padding) * player.hand_size / 2,
                    .y = height - 1.5 * initial_down_size_y,
                };
            },
            .top => {
                return .{
                    .x = width / 2 - initial_down_size_x * player.hand_size / 2,
                    .y = initial_top_y,
                };
            },
        }
    }

    pub fn draw(position: *rl.Vector2, size: rl.Vector2, side: Side) !void {
        switch (side) {
            .left => {
                const initial_x = position.x;
                rl.drawRectangleV(position.*, size, rl.Color.white);
                const size_color: rl.Vector2 = .{
                    .x = 20,
                    .y = size.y,
                };
                const rec: rl.Rectangle = .{
                    .x = position.x,
                    .y = position.y,
                    .width = size.x + size_color.x,
                    .height = size.y,
                };
                position.x += size.x;
                rl.drawRectangleV(position.*, size_color, config.ColorPalette.primary);
                rl.drawRectangleLinesEx(rec, 2, rl.Color.black);
                position.x = initial_x;
                position.y += 40;
            },
            .right => {
                const initial_x = position.x;
                const size_color: rl.Vector2 = .{
                    .x = 20,
                    .y = size.y,
                };
                rl.drawRectangleV(position.*, size_color, config.ColorPalette.primary);
                position.x += size_color.x;
                rl.drawRectangleV(position.*, size, rl.Color.white);
                const rec: rl.Rectangle = .{
                    .x = initial_x,
                    .y = position.y,
                    .width = size.x + size_color.x,
                    .height = size.y,
                };
                rl.drawRectangleLinesEx(rec, 2, rl.Color.black);
                position.x = initial_x;
                position.y += 40;
            },
            .top => {
                const initial_y = position.y;
                rl.drawRectangleV(position.*, size, rl.Color.white);
                position.y += 20;
                const size_color: rl.Vector2 = .{
                    .x = size.x,
                    .y = 40,
                };
                rl.drawRectangleV(position.*, size_color, config.ColorPalette.primary);
                const rec: rl.Rectangle = .{
                    .x = position.x,
                    .y = initial_y,
                    .width = size.x,
                    .height = initial_top_size_y + 10,
                };
                rl.drawRectangleLinesEx(rec, 2, rl.Color.black);
                position.y = initial_y;
                position.x += size.x;
            },
            .down => {
                std.debug.print("First click on tile!", .{});
                return error.called_tile_drawing_in_down;
            },
        }
    }
};
