const assets = @import("assets.zig");
const character = @import("menu/character.zig");
const config = @import("config.zig");
const connection = @import("menu/connection.zig");
const game = @import("game/main.zig");
const mainMenu = @import("menu/main.zig");
const rl = @import("raylib");
const state = @import("game/state.zig");
const std = @import("std");
const user = @import("menu/user.zig");
const zerl = @import("zerl");

pub fn main() anyerror!void {
    rl.setConfigFlags(.{ .window_resizable = true });
    rl.initWindow(@intFromFloat(config.Screen.initialWidth), @intFromFloat(config.Screen.initialHeight), "Lyceum");
    defer rl.closeWindow();
    rl.setTargetFPS(60);
    var node = try zerl.Node.init("lyceum");

    var gameState = try state.init(
        std.heap.c_allocator, // Revisit this later
        config.Screen.initialWidth,
        config.Screen.initialHeight,
        &node,
    );

    // try character.goToSpawn(&gameState);
    mainMenu.spawn(&gameState);
    while (!rl.windowShouldClose()) {
        rl.beginDrawing();
        defer rl.endDrawing();

        rl.clearBackground(config.ColorPalette.background);
        gameState.width = @floatFromInt(rl.getScreenWidth());
        gameState.height = @floatFromInt(rl.getScreenHeight());

        switch (gameState.scene) {
            .user_registry => {
                rl.openURL("https://github.com/Dr-Nekoma/lyceum");
                gameState.scene = .nothing;
            },
            .user_login => {
                try user.login(&gameState);
            },
            .join => {
                try character.join(&gameState);
            },
            .spawn => {
                try game.spawn(&gameState);
            },
            .character_selection => {
                try character.selection(&gameState);
            },
            .connect => {
                try connection.connect(&gameState);
            },
            .nothing => {
                mainMenu.spawn(&gameState);
            },
        }
        connection.status(&gameState);
    }
}
