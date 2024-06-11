const erl = @import("erlang.zig");
const std = @import("std");
const rl = @import("raylib");
const config = @import("config.zig");
const button = @import("components/button.zig");
const text = @import("components/text.zig");

pub fn print_connect_server_error(message: anytype) !void {
    const stdout = std.io.getStdOut().writer();
    try stdout.print(
        "Could not connect to Lyceum Server!\n\u{1b}[31mError: \u{1b}[37m{}\n",
        .{message},
    );
}

pub const Menu = struct {
    pub const Login = struct {
        username: [bufferSize:0]u8 = .{0} ** bufferSize,
        usernamePosition: usize = 0,
        password: [bufferSize:0]u8 = .{0} ** bufferSize,
        passwordPosition: usize = 0,
        pub const bufferSize = 50;
    };
    pub const Configuration = struct {
        pub const ScreenMode = enum {
            windowed,
            fullscreen,
        };
        currentScreenMode: ScreenMode = .windowed,
    };
    login: Login = .{},
    config: Configuration = .{},
};

pub const GameState = struct {
    pub const Scene = enum {
        user_registry,
        user_login,
        nothing,
        game_spawn,
    };
    scene: Scene,
    width: f32,
    height: f32,
    menu: Menu = .{},
    node: *erl.LNode,
    allocator: std.mem.Allocator = std.heap.c_allocator,

    pub fn init(width: f32, height: f32, node: *erl.LNode) !GameState {
        if (width < 0) return error.negative_width;
        if (height < 0) return error.negative_height;
        return .{
            .scene = .nothing,
            .width = width,
            .height = height,
            .node = node,
        };
    }

    fn menuButtonSize(gameState: *const @This()) rl.Vector2 {
        return .{
            .x = gameState.width / 4,
            .y = gameState.height / 8,
        };
    }

    fn userRegistryButton(gameState: *@This()) void {
        const buttonSize = gameState.menuButtonSize();
        const buttonPosition: rl.Vector2 = .{
            .x = (gameState.width / 2) - (buttonSize.x / 2),
            .y = (gameState.height / 2) - (buttonSize.y / 2),
        };
        if (button.at(
            "Create User",
            buttonPosition,
            buttonSize,
            config.ColorPalette.primary,
        )) gameState.scene = .user_registry;
    }

    fn userLoginButton(gameState: *@This()) void {
        const buttonSize = gameState.menuButtonSize();
        const createUserButtonX = (gameState.width / 2) - (buttonSize.x / 2);
        const createUserButtonY = (gameState.height / 2) - (buttonSize.y / 2);
        const buttonPosition: rl.Vector2 = .{
            .x = createUserButtonX,
            .y = createUserButtonY + buttonSize.y + config.menuButtonsPadding,
        };
        if (button.at(
            "Login",
            buttonPosition,
            buttonSize,
            config.ColorPalette.primary,
        )) gameState.scene = .user_login;
    }

    pub fn mainMenu(gameState: *@This()) void {
        userRegistryButton(gameState);
        userLoginButton(gameState);
    }

    pub fn loginScene(gameState: *@This()) void {
        const buttonSize = gameState.menuButtonSize();
        const usernameBoxPosition: rl.Vector2 = .{
            .x = (gameState.width / 2) - (buttonSize.x / 2),
            .y = (gameState.height / 2) - (buttonSize.y / 2),
        };
        const usernameLabelPositionY =
            usernameBoxPosition.y - config.buttonFontSize - 2 * config.menuButtonsPadding;

        rl.drawText(
            "User Name:",
            @intFromFloat(usernameBoxPosition.x),
            @intFromFloat(usernameLabelPositionY),
            config.buttonFontSize,
            rl.Color.white,
        );
        const usernameText = text{
            .content = &gameState.menu.login.username,
            .position = &gameState.menu.login.usernamePosition,
        };
        usernameText.at(usernameBoxPosition);

        const passwordLabelPositionY =
            usernameBoxPosition.y + text.textBoxSize.y + 2 * config.menuButtonsPadding;

        const passwordBoxPosition: rl.Vector2 = .{
            .x = usernameBoxPosition.x,
            .y = passwordLabelPositionY + config.buttonFontSize + 2 * config.menuButtonsPadding,
        };
        rl.drawText(
            "Password:",
            @intFromFloat(passwordBoxPosition.x),
            @intFromFloat(passwordLabelPositionY),
            config.buttonFontSize,
            rl.Color.white,
        );
        const passwordText = text{
            .content = &gameState.menu.login.password,
            .position = &gameState.menu.login.passwordPosition,
        };
        passwordText.at(passwordBoxPosition);

        const buttonPosition: rl.Vector2 = .{
            .x = usernameBoxPosition.x,
            .y = passwordBoxPosition.y + text.textBoxSize.y + 2 * config.menuButtonsPadding,
        };
        if (button.at(
            "Login",
            buttonPosition,
            buttonSize,
            config.ColorPalette.primary,
        )) gameState.scene = .game_spawn;
    }

    pub fn joinGameScene(gameState: *@This()) !void {

        // TODO: Add a timeout for login
        try erl.send_payload(gameState.node, .{
            .user_login = .{
                .username = &gameState.menu.login.username,
                .password = &gameState.menu.login.password,
            },
        });
        // TODO: Add loading animation to wait for response
        const return_type = erl.with_pid(std.meta.Tuple(&.{ [:0]const u8, i64, std.meta.Tuple(&.{ i64, i64 }) }));
        const msg = try erl.receive_message(return_type, gameState.allocator, gameState.node);
        std.debug.print("{}", .{msg.@"1"});
        gameState.scene = .nothing;
    }
};

pub fn main() anyerror!void {
    const connection_status = erl.ei.ei_init();
    if (connection_status != 0) return error.ei_init_failed;
    var node: erl.LNode = try erl.prepare_connection();
    erl.establish_connection(&node) catch |error_value| {
        try print_connect_server_error(error_value);
        std.process.exit(2);
    };

    var gameState = try GameState.init(800, 450, &node);

    rl.setConfigFlags(.flag_window_resizable);
    rl.initWindow(@intFromFloat(gameState.width), @intFromFloat(gameState.height), "Lyceum");
    defer rl.closeWindow();
    rl.setTargetFPS(60);

    GameState.mainMenu(&gameState);
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
                GameState.loginScene(&gameState);
            },
            .game_spawn => {
                try GameState.joinGameScene(&gameState);
            },
            .nothing => {
                GameState.mainMenu(&gameState);
            },
        }
    }
}
