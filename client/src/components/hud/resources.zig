const config = @import("../../config.zig");
const GameState = @import("../../game/state.zig");
const messages = @import("../../server/messages.zig");
const rl = @import("raylib");
const std = @import("std");

pub const collect_key: rl.KeyboardKey = .key_r;
pub const collect_key_label = "R";

const ResourceInfo = struct {
    action: [:0]const u8,
    drawer: *const fn (width: f32, height: f32, font: *rl.Font) void,
};

pub fn drawProgressBar(gameState: *GameState, resource: messages.Resource) f64 {
    const width = gameState.width;
    const height = gameState.height;

    const bar_width = width / 5;
    const bar_height = 30;

    const inner_padding = 10;

    const barSize: rl.Vector2 = .{
        .x = bar_width,
        .y = bar_height,
    };

    const barPosition: rl.Vector2 = .{
        .x = width / 2 - barSize.x / 2,
        .y = height - 162,
    };

    const now = rl.getTime();
    const action_updated = gameState.world.character.action_updated;

    const time_collecting = now - action_updated;
    const collection_time_millis: f64 = @floatFromInt(resource.base_extraction_time);
    const collection_time = collection_time_millis / 1000;

    const bar_percentage = std.math.clamp(time_collecting / collection_time, 0.0, 1.0);

    const innerBarSize: rl.Vector2 = .{
        .x = (bar_width - inner_padding) * @as(f32, @floatCast(bar_percentage)),
        .y = bar_height - inner_padding,
    };

    const innerBarPosition: rl.Vector2 = .{
        .x = barPosition.x + inner_padding / 2,
        .y = barPosition.y + inner_padding / 2,
    };
    rl.drawRectangleV(barPosition, barSize, config.ColorPalette.primary);
    rl.drawRectangleV(innerBarPosition, innerBarSize, config.ColorPalette.secondary);

    return bar_percentage;
}

pub const info = std.StaticStringMap(ResourceInfo).initComptime(.{
    .{ @tagName(.rock), .{
        .action = "Mine",
        .drawer = rock,
    } },
    .{ @tagName(.tree), .{
        .action = "Chop",
        .drawer = tree,
    } },
    .{ @tagName(.bush), .{
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

    const keyLabelLength: f32 = rl.measureTextEx(font.*, collect_key_label, config.resourceActionFontSize + 6, config.textSpacing).x;
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
    rl.drawTextEx(font.*, collect_key_label, labelPosition, config.resourceActionFontSize + 6, config.textSpacing, config.ColorPalette.primary);

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
