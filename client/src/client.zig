const assets = @import("assets.zig");
const character = @import("menu/character.zig");
const config = @import("config.zig");
const connection = @import("menu/connection.zig");
const game = @import("game/main.zig");
const mainMenu = @import("menu/main.zig");
const rl = @import("raylib");
const rm = rl.math;
const state = @import("game/state.zig");
const std = @import("std");
const user = @import("menu/user.zig");
const zerl = @import("zerl");
const hud = @import("components/hud/main.zig");
const map = @import("components/hud/map.zig");
const chat = @import("components/hud/Chat.zig");

pub fn main() anyerror!void {
    rl.setConfigFlags(.{ .window_resizable = true });
    rl.initWindow(@intFromFloat(config.Screen.initialWidth), @intFromFloat(config.Screen.initialHeight), "Lyceum");
    defer rl.closeWindow();
    rl.setTargetFPS(60);
    var node = try zerl.Node.init("lyceum");

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

    gameState.world.character.name = "Gaiseric";
    gameState.world.character.level = 15;
    gameState.world.character.health = 75;
    gameState.world.character.mana = 50;
    gameState.world.character.inventory = .{
        .hud = .{
            .spells = &.{ "item1", "item2", "item3", "item4", "item5", "item6", "item7", "item8", "item9", "item10" },
            .consumables = &.{ "item11", "item12" },
            // TODO: use an actual map
            .map = try assets.image("teapot.png"),
            .texture = map.init_map_texture(),
            .chat = .{
                .in = chat{
                    .content = try gameState.allocator.allocSentinel(u8, config.messageSize, 0),
                    .position = &gameState.world.character.inventory.hud.chat.position,
                    .messages = std.ArrayList(chat.Message).init(gameState.allocator),
                },
            },
        },
    };
    @memset(gameState.world.character.inventory.hud.chat.in.content, 0);

    map.add_borders(&gameState.world.character.inventory.hud.map.?);
    // try character.goToSpawn(&gameState);
    mainMenu.spawn(&gameState);
    while (!rl.windowShouldClose()) {
        rl.beginDrawing();
        defer rl.endDrawing();

        const x = &gameState.world.character.stats.x_position;
        const y = &gameState.world.character.stats.y_position;
        if (rl.isKeyDown(.key_w)) {
            y.* -= 1;
            gameState.world.character.faceDirection = 0;
        }
        if (rl.isKeyDown(.key_s)) {
            y.* += 1;
            gameState.world.character.faceDirection = 180;
        }
        if (rl.isKeyDown(.key_a)) {
            x.* -= 1;
            gameState.world.character.faceDirection = 270;
        }
        if (rl.isKeyDown(.key_d)) {
            x.* += 1;
            gameState.world.character.faceDirection = 90;
        }
        x.* = @intFromFloat(rm.clamp(@floatFromInt(x.*), 0, config.map.max_width));
        y.* = @intFromFloat(rm.clamp(@floatFromInt(y.*), 0, config.map.max_height));
        rl.clearBackground(config.ColorPalette.background);
        gameState.width = @floatFromInt(rl.getScreenWidth());
        gameState.height = @floatFromInt(rl.getScreenHeight());

        try hud.at(&gameState.world.character, gameState.width, gameState.height);
        // switch (gameState.scene) {
        //     .user_registry => {
        //         rl.openURL("https://github.com/Dr-Nekoma/lyceum");
        //         gameState.scene = .nothing;
        //     },
        //     .user_login => {
        //         try user.login(&gameState);
        //     },
        //     .join => {
        //         try character.join(&gameState);
        //     },
        //     .spawn => {
        //         try game.spawn(&gameState);
        //     },
        //     .character_selection => {
        //         try character.selection(&gameState);
        //     },
        //     .connect => {
        //         try connection.connect(&gameState);
        //     },
        //     .nothing => {
        //         mainMenu.spawn(&gameState);
        //     },
        // }
        connection.status(&gameState);
    }
}
