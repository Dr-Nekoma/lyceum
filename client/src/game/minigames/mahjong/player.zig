const rl = @import("raylib");
const rm = rl.math;
const tiles = @import("tiles.zig");
const std = @import("std");

pub const hand_size = 13;

pub const Phase = enum {
    draw,
    action,
    discard,

    pub fn next(current: @This()) @This() {
        return switch (current) {
            .draw => .action,
            .action => .discard,
            .discard => .draw,
        };
    }
};

pub const Entity = struct {
    name: [:0]const u8 = "Tester",
    // discard_pile: []tiles.Tile,
    hand: [hand_size]tiles.Tile,
    drawn_tile: ?tiles.Tile = null,
    discarded: [tiles.pile_size]tiles.Tile,
    discarded_counter: u8 = 0,
    points: i16 = 0,
    yaku: u8 = 0, // How many points that you have in your hand that enable you to win the game
    // TODO profile_picture: rl.Texture,
    phase: Phase = .draw,
};

pub const Action = enum {
    tsumo,
    // ron Other player hand
    khan,
    pon,
    riichi,
    chii,
};

pub const Target = enum {
    pile,
    player,
};

pub fn draw(p: *Entity, pile: []tiles.Tile) ![]tiles.Tile {
    if (pile.len == 0) return error.empty_pile;

    p.drawn_tile = pile[0];

    return pile[1..];
}

pub fn discard(p: *Entity, discarded_index: usize) void {
    // TODO: discarded_index should be an enum
    const current_hand_size = p.hand.len;
    const discarded_tile = if (discarded_index > current_hand_size) p.drawn_tile.? else p.hand[discarded_index];
    p.discarded[p.discarded_counter] = discarded_tile;
    p.discarded_counter += 1;
    std.debug.print("You are suppose to have discarded {}!\n", .{discarded_index});
}
