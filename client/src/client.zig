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
    rl.initAudioDevice();
    defer rl.closeAudioDevice();

    rl.setTargetFPS(60);

    var menuAssets = try mainMenu.loadAssets();

    var errorElem = errorC{
        .sound = &menuAssets.sounds.error_sound,
        .font = &menuAssets.font,
    };
    var node = try zerl.Node.init("lyceum");
    var gameState = try state.init(
        std.heap.c_allocator, // Revisit this later
        config.Screen.initialWidth,
        config.Screen.initialHeight,
        &node,
        &errorElem,
        &menuAssets,
    );

    rl.playMusicStream(gameState.menu.assets.music);
    while (!rl.windowShouldClose()) {
        music.play(&gameState);
        rl.beginDrawing();
        defer rl.endDrawing();

        rl.clearBackground(config.ColorPalette.background);
        gameState.width = @floatFromInt(rl.getScreenWidth());
        gameState.height = @floatFromInt(rl.getScreenHeight());

        switch (gameState.scene) {
            .user_registry => {
                mainMenu.manageBackground(&gameState);
                gameState.errorElem.update(.create_account_not_implemented);
                gameState.scene = .nothing;
            },
            .user_login => {
                mainMenu.manageBackground(&gameState);
                mainMenu.displayLogo(&gameState);
                try userMenu.login(&gameState);
            },
            .spawn => {
                try inGame.spawn(&gameState);
                try hud.at(&gameState);
            },
            .character_selection => {
                rl.drawTextureEx(gameState.menu.assets.backgrounds.character_selection, .{ .x = 0, .y = 0 }, 0, 1, rl.Color.white);
                try characterMenu.selection(&gameState);
            },
            .connect => {
                mainMenu.manageBackground(&gameState);
                mainMenu.displayLogo(&gameState);
                try connectionMenu.connect(&gameState);
            },
            .nothing => {
                mainMenu.manageBackground(&gameState);
                mainMenu.displayLogo(&gameState);
                try mainMenu.spawn(&gameState);
            },
        }
        connectionMenu.status(&gameState);
        gameState.errorElem.at(gameState.width, gameState.height);
        gameState.menu.main.back_button.at(&gameState.scene, gameState.height);
        music.control(&gameState);
    }
    music.stop(&gameState);
}
