const spells = @import("spells.zig");
const consumables = @import("consumables.zig");
const GameState = @import("../../game/state.zig");

pub fn at(
    character: GameState.World.Character,
    width: f32,
    height: f32,
) !void {
    try spells.at(character.inventory.hud.spells, width, height);
    try consumables.at(character.inventory.hud.consumables, height);
}
