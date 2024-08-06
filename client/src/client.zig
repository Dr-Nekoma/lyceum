const erl = @import("erlang/config.zig");
const std = @import("std");
const rl = @import("raylib");
const config = @import("config.zig");
const state = @import("game/state.zig");
const user = @import("menu/user.zig");
const character = @import("menu/character.zig");
const mainMenu = @import("menu/main.zig");
const game = @import("game/main.zig");

pub fn print_connect_server_error(message: anytype) !void {
    const stdout = std.io.getStdOut().writer();
    try stdout.print(
        "Could not connect to Lyceum Server!\n\u{1b}[31mError: \u{1b}[37m{}\n",
        .{message},
    );
}

pub fn main() anyerror!void {
    const connection_status = erl.ei.ei_init();
    if (connection_status != 0) return error.ei_init_failed;
    var node: erl.Node = try erl.prepare_connection();
    erl.establish_connection(&node) catch |error_value| {
        try print_connect_server_error(error_value);
        std.process.exit(2);
    };

    var gameState = try state.init(800, 450, &node);
    gameState.menu = .{ .character_name = try gameState.allocator.allocSentinel(u8, config.nameSize, 0) };
    @memset(gameState.menu.character_name, 0);

    rl.setConfigFlags(.flag_window_resizable);
    rl.initWindow(@intFromFloat(gameState.width), @intFromFloat(gameState.height), "Lyceum");
    defer rl.closeWindow();
    rl.setTargetFPS(60);

    try character.goToSpawn(&gameState);
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
            .nothing => {
                mainMenu.spawn(&gameState);
            },
        }
    }
}
