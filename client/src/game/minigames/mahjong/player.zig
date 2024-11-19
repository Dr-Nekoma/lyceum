const rl = @import("raylib");
const rm = rl.math;
const tiles = @import("tiles.zig");

pub const hand_size = 15;

pub const State = enum {
    idle,
    riichi,
};

pub const Entity = struct {
    name: [:0]const u8 = "Tester",
    // discard_pile: []tiles.Tile,
    hand: [hand_size]tiles.Tile,
    points: i16 = 0,
    yaku: u8 = 0, // How many points that you have in your hand that enable you to win the game
    state: State = .idle,
    // TODO profile_picture: rl.Texture,
};
