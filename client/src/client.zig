const assets = @import("assets.zig");
const character = @import("menu/character.zig");
const connection = @import("menu/connection.zig");
const config = @import("config.zig");
const erl = @import("erlang.zig");
const game = @import("game/main.zig");
const mainMenu = @import("menu/main.zig");
const rl = @import("raylib");
const std = @import("std");
const state = @import("game/state.zig");
const user = @import("menu/user.zig");

pub fn main() anyerror!void {
    rl.setConfigFlags(.{ .window_resizable = true });
    rl.initWindow(@intFromFloat(config.Screen.initialWidth), @intFromFloat(config.Screen.initialHeight), "Lyceum");
    defer rl.closeWindow();
    rl.setTargetFPS(60);

    var node: erl.Node = try erl.Node.init();
    var gameState = try state.init(config.Screen.initialWidth, config.Screen.initialHeight, &node);
    gameState.menu = .{
        .character = .{
            .create = .{
                .name = try gameState.allocator.allocSentinel(u8, config.nameSize, 0),
            },
        },
        .assets = try mainMenu.loadAssets(),
    };
    @memset(gameState.menu.character.create.name, 0);

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
