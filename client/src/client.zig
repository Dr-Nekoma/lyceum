const assets = @import("assets.zig");
const Button = @import("components/button.zig");
const characterMenu = @import("menu/character.zig");
const config = @import("config.zig");
const connectionMenu = @import("menu/connection.zig");
const errorC = @import("components/error.zig");
const inGame = @import("game/main.zig");
const hud = @import("components/hud/main.zig");
const mainMenu = @import("menu/main.zig");
const music = @import("music.zig");
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
    // rl.initAudioDevice();
    // defer rl.closeAudioDevice();
    // const musicX = try assets.music(config.assets.paths.menu.music.background);
    // rl.playMusicStream(musicX);
    while (!rl.windowShouldClose()) {
        // music.play(&gameState);
        // rl.updateMusicStream(musicX);
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
                mainMenu.displayLogo(&gameState);
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
                mainMenu.displayLogo(&gameState);
                try connectionMenu.connect(&gameState);
            },
            .nothing => {
                mainMenu.displayLogo(&gameState);
                try mainMenu.spawn(&gameState);
            },
        }
        connectionMenu.status(&gameState);
        gameState.errorElem.at(gameState.width, gameState.height);
        Button.Clickable.Back.at(&gameState.scene, gameState.height);
        // music.control(&gameState);
    }
    // rl.stopMusicStream(gameState.menu.assets.music);
    // rl.unloadMusicStream(gameState.menu.assets.music);
}
