const config = @import("../../config.zig");
const rl = @import("raylib");
const std = @import("std");

const ResourceInfo = struct {
    key: rl.KeyboardKey,
    key_label: [:0]const u8,
    action: [:0]const u8,
    drawer: *const fn (width: f32, height: f32, font: *rl.Font) void,
};

pub const info = std.StaticStringMap(ResourceInfo).initComptime(.{
    .{ @tagName(.rock), .{
        .key = .key_r,
        .key_label = "R",
        .action = "Mine",
        .drawer = rock,
    } },
    .{ @tagName(.tree), .{
        .key = .key_r,
        .key_label = "R",
        .action = "Chop",
        .drawer = tree,
    } },
    .{ @tagName(.bush), .{
        .key = .key_r,
        .key_label = "R",
        .action = "Harvest",
        .drawer = bush,
    } },
});

fn generic(key: [:0]const u8, width: f32, height: f32, font: *rl.Font) void {
    const value = info.get(key).?;
    const actionLength: rl.Vector2 = rl.measureTextEx(font.*, value.action, config.resourceActionFontSize, config.textSpacing);

    const boundarySize: rl.Vector2 = .{
        .x = 20 + actionLength.x,
        .y = actionLength.y + 14,
    };

    const keyLabelLength: f32 = rl.measureTextEx(font.*, value.key_label, config.resourceActionFontSize + 6, config.textSpacing).x;
    const labelBoxSize: rl.Vector2 = .{
        .x = keyLabelLength + 10,
        .y = actionLength.y + 14,
    };

    const labelBoxPosition: rl.Vector2 = .{
        .x = width / 2 - boundarySize.x / 2 - labelBoxSize.x / 2,
        .y = height - 162,
    };

    const labelPosition: rl.Vector2 = .{
        .x = labelBoxPosition.x + 5,
        .y = labelBoxPosition.y + 7,
    };

    const boundaryPosition: rl.Vector2 = .{
        .x = labelBoxPosition.x + labelBoxSize.x,
        .y = height - 162,
    };

    rl.drawRectangleV(boundaryPosition, boundarySize, config.ColorPalette.primary);
    rl.drawRectangleLinesEx(.{ .x = boundaryPosition.x, .y = boundaryPosition.y, .width = boundarySize.x, .height = boundarySize.y }, 2, config.ColorPalette.secondary);
    rl.drawRectangleV(labelBoxPosition, labelBoxSize, config.ColorPalette.secondary);
    rl.drawTextEx(font.*, value.key_label, labelPosition, config.resourceActionFontSize + 6, config.textSpacing, config.ColorPalette.primary);

    const actionPosition: rl.Vector2 = .{
        .x = boundaryPosition.x + 10,
        .y = boundaryPosition.y + 7,
    };
    rl.drawTextEx(font.*, value.action, actionPosition, config.resourceActionFontSize, config.textSpacing, config.ColorPalette.secondary);
}

fn rock(width: f32, height: f32, font: *rl.Font) void {
    generic("rock", width, height, font);
}

fn tree(width: f32, height: f32, font: *rl.Font) void {
    generic("tree", width, height, font);
}

fn bush(width: f32, height: f32, font: *rl.Font) void {
    generic("bush", width, height, font);
}
