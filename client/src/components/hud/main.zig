const chat = @import("Chat.zig");
const config = @import("../../config.zig");
const spells = @import("spells.zig");
const map = @import("map.zig");
const consumables = @import("consumables.zig");
const info = @import("info.zig");
const GameState = @import("../../game/state.zig");
const rl = @import("raylib");
const rm = rl.math;
const std = @import("std");

fn drawPlayers(gameState: *GameState) !void {
    var player_iterator = gameState.world.other_players.valueIterator();
    while (player_iterator.next()) |player| {
        var infoPosition = rl.getWorldToScreen(player.position, gameState.world.camera);
        infoPosition.y += 30;
        const fontSize = 15;
        try info.at(player, info.mainSize, infoPosition, fontSize, gameState.allocator);

        const map_image = gameState.world.character.inventory.hud.minimap.map.?;
        const x, const y = map.coordinates.normalize(player, &map_image, &gameState.world.map);
        const center: rl.Vector2 = map.getCenter(gameState.width, gameState.height).add(.{ .x = x, .y = y });
        std.debug.print("Other Player", .{});
        map.player(player, center);
    }
}

pub fn at(gameState: *GameState) !void {
    const width = gameState.width;
    const height = gameState.height;
    const character = &gameState.world.character;

    try spells.at(character.inventory.hud.spells, width, height);

    try consumables.at(character.inventory.hud.consumables, height);

    const mainPosition: rl.Vector2 = .{
        .x = width / 2,
        .y = 3 * config.menuButtonsPadding,
    };

    try info.at(character, info.mainSize, mainPosition, config.textFontSize, gameState.allocator);

    try map.at(character, &gameState.world.map, width, height);

    const chatC = chat{
        .content = &character.inventory.hud.chat.content,
        .position = &character.inventory.hud.chat.position,
        .messages = &character.inventory.hud.chat.messages,
        .mode = &character.inventory.hud.chat.mode,
    };
    try chatC.at(character.stats.name, gameState);

    try drawPlayers(gameState);
}
