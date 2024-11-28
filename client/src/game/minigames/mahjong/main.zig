const config = @import("../../../config.zig");
const Button = @import("../../../components/button.zig");
const rl = @import("raylib");
const rm = rl.math;
const mainMenu = @import("../../../menu/main.zig");
const tiles = @import("tiles.zig");
const rules = @import("rules.zig");
const player = @import("player.zig");
const GameState = @import("../../state.zig");
const std = @import("std");

pub fn drawOtherPlayers(gameState: *GameState) !void {
    const players = gameState.mahjong.players;
    const width = gameState.width;
    const height = gameState.height;

    const directions: [3]tiles.drawing.Side = .{ .left, .top, .right };

    for (players, directions) |p, direction| {
        var position = tiles.drawing.defaultPosition(width, height, direction);
        const size = tiles.drawing.defaultSize(direction);
        for (p.hand) |_| try tiles.drawing.draw(&position, size, direction);
    }
}

pub fn drawTileButton(index: usize, _: tiles.Tile, b: Button.Clickable, position: rl.Vector2) ?usize {
    if (b.at(
        "-",
        position,
        tiles.tile_size,
        config.ColorPalette.primary,
    )) {
        std.debug.print("First click on tile!", .{});
        return index;
    }
    return null;
}

pub const MahjongState = struct {
    // players: [3]player.Entity,
    player: struct {
        buttons: [player.hand_size]Button.Clickable,
        entity: player.Entity,
    },
    pile: tiles.Pile,
    pile_start: usize = 0,
    turn: enum {
        ours,
        others,
    } = .ours,

    pub fn init(assets: *mainMenu.Menu.Assets) @This() {
        var pile = tiles.default_pile;
        std.crypto.random.shuffle(tiles.Tile, &pile);

        var position: usize = 0;

        var p: player.Entity = .{
            .hand = undefined,
            .discarded = undefined,
        };

        for (&p.hand) |*tile| {
            tile.* = pile[position];
            position += 1;
        }

        var tile_buttons: [player.hand_size]Button.Clickable = undefined;
        for (0..player.hand_size) |i| {
            tile_buttons[i] = Button.Clickable{
                .font = &assets.font,
                .sound = &assets.sounds.buttons.click,
            };
        }
        return .{
            .pile = pile,
            .pile_start = position,
            .player = .{
                .entity = p,
                .buttons = tile_buttons,
            },
        };
    }
};

pub fn main(gameState: *GameState) !void {
    const width = gameState.width;
    const height = gameState.height;

    var position: rl.Vector2 = tiles.drawing.defaultPosition(width, height, .down);
    const size = tiles.drawing.defaultSize(.down);
    const p = &gameState.mahjong.player.entity;
    const buttons = &gameState.mahjong.player.buttons;

    const discard = p.phase == .discard;
    const disabled = !(gameState.mahjong.turn == .ours and discard);

    var advance = false;
    for (0..player.hand_size) |i| {
        buttons[i].disabled = disabled;
        if (drawTileButton(i, p.hand[i], buttons[i], position)) |clicked_index| {
            player.discard(p, clicked_index);
            advance = true;
        }
        position.x += size.x + 15;
    }

    if (advance or !discard) p.phase = p.phase.next();

    // try drawOtherPlayers(gameState);
}
