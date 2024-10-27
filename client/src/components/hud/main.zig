const chat = @import("Chat.zig");
const spells = @import("spells.zig");
const map = @import("map.zig");
const consumables = @import("consumables.zig");
const info = @import("info.zig");
const GameState = @import("../../game/state.zig");
const std = @import("std");

pub fn at(
    character: *GameState.World.Character,
    width: f32,
    _: f32,
) !void {
    // try spells.at(character.*.inventory.hud.spells, width, height);
    // try consumables.at(character.*.inventory.hud.consumables, height);
    try info.at(character.*, width);
    // try map.at(character.*, width, height);
    // const chatC = chat{
    //     .content = &character.inventory.hud.chat.content,
    //     .position = &character.inventory.hud.chat.position,
    //     .messages = &character.inventory.hud.chat.messages,
    //     .mode = &character.inventory.hud.chat.mode,
    // };
    // try chatC.at(character.stats.name, width, height);
    // for (character.*.inventory.hud.chat.in.messages.items) |message| {
    //     std.debug.print("Author: {s} | Message: {s}\n", .{ message.author, message.content });
    // }
    // std.debug.print("\n\n", .{});
}
