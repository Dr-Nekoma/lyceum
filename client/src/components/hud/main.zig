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
    const mainPlayer = &gameState.world.character;
    var player_iterator = gameState.world.other_players.valueIterator();
    while (player_iterator.next()) |player| {
        if (GameState.canDisplayPlayer(mainPlayer, player)) {
            var infoPosition = rl.getWorldToScreen(player.position, gameState.world.camera);
            infoPosition.y += 30;
            const fontSize = 15;
            try info.stats(player, info.mainSize, infoPosition, fontSize, gameState.allocator, &gameState.menu.assets.font);
        }
        const character = gameState.world.character;
        const map_image = gameState.world.character.inventory.hud.minimap.map.?;
        const c_x, const c_y = map.coordinates.normalize(.{
            .x = character.stats.x_position,
            .y = character.stats.y_position,
        }, &map_image, &gameState.world.map);
        const p_x, const p_y = map.coordinates.normalize(.{
            .x = player.stats.x_position,
            .y = player.stats.y_position,
        }, &map_image, &gameState.world.map);
        const delta: rl.Vector2 = .{
            .x = p_x - c_x,
            .y = p_y - c_y,
        };
        if (delta.length() < map.innerRadius) {
            const center: rl.Vector2 = map.getCenter(gameState.width, gameState.height).add(delta);
            map.player(player.stats.face_direction, center);
        }
    }
}

pub fn at(gameState: *GameState) !void {
    const width = gameState.width;
    const height = gameState.height;
    const character = &gameState.world.character;

    try spells.at(character.inventory.hud.spells, width, height, &gameState.menu.assets.font);

    try consumables.at(character.inventory.hud.consumables, height, &gameState.menu.assets.font);

    const mainPosition: rl.Vector2 = .{
        .x = width / 2,
        .y = 18,
    };

    try info.at(character, info.mainSize, mainPosition, config.textFontSize, gameState.allocator, &gameState.menu.assets.font);

    try map.at(character, &gameState.world.map, width, height, &gameState.menu.assets.font);

    const chatC = chat{
        .content = &character.inventory.hud.chat.content,
        .position = &character.inventory.hud.chat.position,
        .messages = &character.inventory.hud.chat.messages,
        .mode = &character.inventory.hud.chat.mode,
    };
    try chatC.at(character.stats.name, gameState);

    // TODO: This should be at the beginning, but mini-map screw us over.
    // Putting this on the top makes the other players pointers disappear.
    try drawPlayers(gameState);
}
