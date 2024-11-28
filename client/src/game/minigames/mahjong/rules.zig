const config = @import("../../../config.zig");
const Button = @import("../../../components/button.zig");
const rl = @import("raylib");
const rm = rl.math;
const tiles = @import("tiles.zig");
const player = @import("player.zig");
const GameState = @import("../../state.zig");
const std = @import("std");
const stdlib = @cImport({
    @cInclude("stdlib.h");
});
