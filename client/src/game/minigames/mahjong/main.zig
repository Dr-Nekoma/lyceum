const config = @import("../../../config.zig");
const Button = @import("../../../components/button.zig");
const rl = @import("raylib");
const rm = rl.math;
const tiles = @import("tiles.zig");
const player = @import("player.zig");
const GameState = @import("../../state.zig");
const std = @import("std");

pub fn drawOtherPlayers(gameState: *GameState, players: [3]player.Entity) !void {
    const width = gameState.width;
    const height = gameState.height;

    const directions: [3]tiles.drawing.Side = .{ .left, .top, .right };

    for (players, directions) |p, direction| {
        var position = tiles.drawing.defaultPosition(width, height, direction);
        const size = tiles.drawing.defaultSize(direction);
        for (p.hand) |_| try tiles.drawing.draw(&position, size, direction);
    }
}

pub fn drawTileButton(_: tiles.Tile, b: Button.Clickable, position: rl.Vector2) void {
    if (b.at(
        "-",
        position,
        tiles.tile_size,
        config.ColorPalette.primary,
    )) {
        std.debug.print("First click on tile!", .{});
    }
}

pub fn drawPlayer(gameState: *GameState) !void {
    const p: player.Entity = .{
        .hand = undefined,
    };
    var tile_buttons: [player.hand_size]Button.Clickable = undefined;
    for (0..player.hand_size) |i| {
        tile_buttons[i] = Button.Clickable{
            .font = &gameState.menu.assets.font,
            .sound = &gameState.menu.assets.sounds.buttons.click,
        };
    }
    const width = gameState.width;
    const height = gameState.height;

    var position: rl.Vector2 = tiles.drawing.defaultPosition(width, height, .down);
    const size = tiles.drawing.defaultSize(.down);
    for (0..player.hand_size) |i| {
        drawTileButton(p.hand[i], tile_buttons[i], position);
        position.x += size.x + 15;
    }
    try drawOtherPlayers(gameState, .{ p, p, p });
}
