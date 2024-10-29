const Button = @import("components/button.zig");
const characterMenu = @import("menu/character.zig");
const config = @import("config.zig");
const connectionMenu = @import("menu/connection.zig");
const errorC = @import("components/error.zig");
const inGame = @import("game/main.zig");
const hud = @import("components/hud/main.zig");
const mainMenu = @import("menu/main.zig");
const rl = @import("raylib");
const server = @import("server/main.zig");
const state = @import("game/state.zig");
const std = @import("std");
const userMenu = @import("menu/user.zig");
const zerl = @import("zerl");

pub fn main() anyerror!void {
    rl.setConfigFlags(.{ .window_resizable = true });
    rl.initWindow(@intFromFloat(config.Screen.initialWidth), @intFromFloat(config.Screen.initialHeight), "Lyceum");
    defer rl.closeWindow();
    rl.setTargetFPS(60);
    var errorElem = errorC{};
    var node = try zerl.Node.init("lyceum");

    var gameState = try state.init(
        std.heap.c_allocator, // Revisit this later
        config.Screen.initialWidth,
        config.Screen.initialHeight,
        &node,
        &errorElem,
    );

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
                try userMenu.login(&gameState);
            },
            .spawn => {
                try inGame.spawn(&gameState);
                try hud.at(&gameState);
            },
            .character_selection => {
                try characterMenu.selection(&gameState);
            },
            .connect => {
                try connectionMenu.connect(&gameState);
            },
            .nothing => {
                try mainMenu.spawn(&gameState);
            },
        }
        connectionMenu.status(&gameState);
        gameState.errorElem.at(gameState.width, gameState.height);
        Button.Clickable.Back.at(&gameState.scene, gameState.height);
    }
}
