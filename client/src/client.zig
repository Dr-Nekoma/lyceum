const erl = @import("erlang/config.zig");
const sender = @import("erlang/sender.zig");
const receiver = @import("erlang/receiver.zig");
const messages = @import("server_messages.zig");
const std = @import("std");
const rl = @import("raylib");
const config = @import("config.zig");
const attribute = @import("components/attribute.zig");
const button = @import("components/button.zig");
const slider = @import("components/slider.zig");
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
    character_name: [:0]u8,
    login: Login = .{},
    email: [:0]const u8 = "",
    config: Configuration = .{},
};

pub const GameState = struct {
    pub const Scene = enum {
        user_registry,
        user_login,
        nothing,
        spawn,
        character_selection,
    };
    scene: Scene,
    width: f32,
    height: f32,
    menu: Menu = undefined,
    node: *erl.Node,
    allocator: std.mem.Allocator = std.heap.c_allocator,
    current_character: messages.Erlang_Character = .{},
    character_list: []const messages.Character = &.{},
    test_value: usize = 0,

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
            .x = gameState.width / 4,
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

    // TODO: add limit for total number of points when creating a character
    // TODO: add create button available to click when character is valid
    fn emptyCharacterScene(gameState: *@This()) !void {
        var currentTextPosition: rl.Vector2 = .{
            .x = 50,
            .y = 150,
        };
        const textSize: rl.Vector2 = .{
            .x = 25,
            .y = 25,
        };
        const fieldPadding = 25;
        inline for (std.meta.fields(messages.Erlang_Character)) |field| {
            if (comptime (!std.mem.eql(u8, field.name, "name") and !std.mem.eql(u8, field.name, "map_name"))) {
                var mutable_name: [:0]u8 = try gameState.allocator.allocSentinel(u8, field.name.len, 0);
                std.mem.copyForwards(u8, mutable_name, field.name);
                mutable_name[0] = std.ascii.toUpper(mutable_name[0]);
                const attributeComp = attribute{
                    .current = &@field(gameState.current_character, field.name),
                    .text = mutable_name,
                    .textPosition = .{
                        .x = currentTextPosition.x,
                        .y = currentTextPosition.y,
                    },
                    .textSize = textSize,
                    .textColor = rl.Color.white,
                };
                try attributeComp.at();
                currentTextPosition.y += textSize.y + fieldPadding;
            } else {
                const nameBoxPosition: rl.Vector2 = .{
                    .x = 50,
                    .y = 50,
                };
                const nameLabelPositionY =
                    nameBoxPosition.y - config.buttonFontSize - 2 * config.menuButtonsPadding;

                rl.drawText(
                    "Name:",
                    @intFromFloat(nameBoxPosition.x),
                    @intFromFloat(nameLabelPositionY),
                    config.buttonFontSize,
                    rl.Color.white,
                );
                const nameText = text{
                    .content = gameState.menu.character_name,
                    .position = &gameState.test_value,
                };
                nameText.at(nameBoxPosition);
                gameState.current_character.name = gameState.menu.character_name;
            }
        }
    }

    pub fn characterSelectionScene(gameState: *@This()) !void {
        const buttonSize = gameState.characterButtonSize();
        const characterButtonY = (gameState.height / 10) - (buttonSize.y / 2);
        var buttonPosition: rl.Vector2 = .{
            .x = buttonSize.x / 4.0,
            .y = characterButtonY,
        };

        var texturePosition: rl.Vector2 = .{
            .x = buttonPosition.x + buttonSize.x / 2 - 150,
            .y = buttonPosition.y + buttonSize.y * 1.5,
        };

        if (gameState.character_list.len != 0) {
            // TODO: Make pagination for 3 characters at a time
            for (gameState.character_list) |character| {
                if (button.at(
                    character.character_data.name,
                    buttonPosition,
                    buttonSize,
                    config.ColorPalette.primary,
                )) {
                    gameState.current_character = character.character_data;
                    gameState.scene = .nothing;
                    break;
                }

                rl.drawTextureEx(character.equipment_data, texturePosition, 0.0, 1, rl.Color.white);

                buttonPosition.x += 5.0 * buttonSize.x / 4.0;
                texturePosition.x += 5.0 * buttonSize.x / 4.0;
            }
        } else {
            // std.debug.print("There are no characters for this user bruh xD", .{});
            try emptyCharacterScene(gameState);
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
            gameState.scene = .spawn;
        }
    }

    pub fn joinGameScene(gameState: *@This()) !void {
        const msg = try messages.receive_login_response(gameState.allocator, gameState.node);
        switch (msg) {
            .ok => |email| {
                gameState.menu.email = email;
                try messages.send_payload(gameState.node, .{
                    .character_list = .{
                        .username = &gameState.menu.login.username,
                        .email = gameState.menu.email,
                    },
                });
                std.debug.print("We asked for characters", .{});
                const maybe_characters = try messages.receive_characters_list(gameState.allocator, gameState.node);
                switch (maybe_characters) {
                    .ok => |erlang_characters| {

                        // TODO: Discover how to make this work
                        // const teapotEmbed = @embedFile("../assets/teapot.png");
                        // const teapotLoaded = rl.loadImageFromMemory(".png", teapotEmbed, teapotEmbed.len);

                        const teapotImage = rl.loadImage("./assets/teapot.png");

                        var characters = std.ArrayList(messages.Character).init(gameState.allocator);

                        for (erlang_characters) |character| {
                            try characters.append(.{
                                .character_data = character,
                                .equipment_data = rl.loadTextureFromImage(teapotImage),
                            });
                        }

                        gameState.character_list = characters.items;
                        gameState.scene = .character_selection;
                    },
                    .@"error" => |error_msg| {
                        std.debug.print("ERROR IN SERVER: {s}", .{error_msg});
                        gameState.scene = .nothing;
                    },
                }
            },
            .@"error" => |error_msg| {
                std.debug.print("ERROR IN SERVER: {s}", .{error_msg});
                gameState.scene = .nothing;
            },
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
    gameState.menu = .{ .character_name = try gameState.allocator.allocSentinel(u8, config.nameSize, 0) };

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
            .spawn => {
                try GameState.joinGameScene(&gameState);
            },
            .character_selection => {
                try GameState.characterSelectionScene(&gameState);
            },
            .nothing => {
                GameState.mainMenu(&gameState);
            },
        }
    }
}
