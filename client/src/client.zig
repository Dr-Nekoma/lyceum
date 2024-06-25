const erl = @import("erlang/config.zig");
const sender = @import("erlang/sender.zig");
const receiver = @import("erlang/receiver.zig");
const messages = @import("server_messages.zig");
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
        game_character_selection,
    };
    scene: Scene,
    width: f32,
    height: f32,
    menu: Menu = .{},
    node: *erl.Node,
    allocator: std.mem.Allocator = std.heap.c_allocator,
    current_character: messages.Erlang_Character = .{
        .name = "Lemos",
        .constitution = 10,
        .wisdom = 10,
        .endurance = 10,
        .strength = 10,
        .intelligence = 10,
        .faith = 10,    
    },
    // character_list: []const Erlang,

    pub fn init(width: f32, height: f32, node: *erl.Node) !GameState {
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

    fn characterButtonSize(gameState: *const @This()) rl.Vector2 {
        return .{
            .x = gameState.width / 6,
            .y = gameState.height / 10,
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

    pub fn characterSelectionScene(gameState: *@This()) !void {
        // TODO: This should not be the current character. Waiting for the Erlang Server
        const characters: []const messages.Erlang_Character =
            &.{gameState.current_character,
               .{
                   .name = "Magueta",
                   .constitution = 2,
                   .wisdom = 8,
                   .endurance = 4,
                   .strength = 4,
                   .intelligence = 8,
                   .faith = 10,    
                },};
        
        const buttonSize = gameState.characterButtonSize();
        const characterButtonX = (gameState.width / 10) + (buttonSize.x / 2) - 20 * config.menuButtonsPadding;
        const characterButtonY = (gameState.height / 10) + (buttonSize.y / 2) - 25 * config.menuButtonsPadding;
        var buttonPosition: rl.Vector2 = .{
            .x = characterButtonX,
            .y = characterButtonY + buttonSize.y + config.menuButtonsPadding,
        };

        var texturePosition: rl.Vector2 = .{
                .x = buttonPosition.x,
                .y = characterButtonY + buttonSize.y + 20 * config.menuButtonsPadding,
            };
        
        for (characters) |character| {
            if (button.at(
                character.name,
                buttonPosition,
                buttonSize,
                config.ColorPalette.primary,
            )) {
                gameState.current_character = character;
                gameState.scene = .nothing;
                break;
            }
            const image: rl.Image = rl.loadImage("../assets/teapot.png");
            const texture: rl.Texture2D = rl.loadTextureFromImage(image);          // Image converted to texture, GPU memory (VRAM)

            rl.drawTextureEx(texture, texturePosition, 0.0, 0.5, rl.Color.white);

            buttonPosition.x += buttonSize.x + 10 * config.menuButtonsPadding;
            texturePosition.x = buttonPosition.x;
        }
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

    pub fn loginScene(gameState: *@This()) !void {
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
        )) {
            // TODO: Add loading animation to wait for response
            // TODO: Add a timeout for login
            try messages.send_payload(gameState.node, .{
                .user_login = .{
                    .username = &gameState.menu.login.username,
                    .password = &gameState.menu.login.password,
                },
            });
            gameState.scene = .game_spawn;
        }
    }
    
    pub fn joinGameScene(gameState: *@This()) !void {
        const msg = try messages.receive_simple_response(gameState.allocator, gameState.node);
        switch (msg) {
            .ok => {
                // TODO: Wait for Erlang Server to be ready
                // gameState.character_list = try messages.receive_characters_list(gameState.allocator, gameState.node);
                const maybe_characters: messages.Erlang_Characters = .{ .ok = &.{gameState.current_character}};
                switch (maybe_characters) {
                    .ok => {
                        gameState.scene = .game_character_selection;
                    },
                    .empty => {
                        std.debug.print("There are no characters for this user bruh xD", .{});
                        gameState.scene = .nothing;                
                    },
                    .@"error" => |error_msg| {
                        std.debug.print("ERROR IN SERVER: {s}", .{error_msg});
                        gameState.scene = .nothing;                
                    }
                }
            },
            .@"error" => |error_msg| {
                std.debug.print("ERROR IN SERVER: {s}", .{error_msg});
                gameState.scene = .nothing;                
            }
        }
    }
};

pub fn main() anyerror!void {
    const connection_status = erl.ei.ei_init();
    if (connection_status != 0) return error.ei_init_failed;
    var node: erl.Node = try erl.prepare_connection();
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
                try GameState.loginScene(&gameState);
            },
            .game_spawn => {
                try GameState.joinGameScene(&gameState);
            },
            .game_character_selection => {
                try GameState.characterSelectionScene(&gameState);
            },
            .nothing => {
                GameState.mainMenu(&gameState);
            },
        }
    }
}
