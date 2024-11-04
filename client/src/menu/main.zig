const assets = @import("../assets.zig");
const config = @import("../config.zig");
const rl = @import("raylib");
const server = @import("../server/main.zig");
const std = @import("std");
const Button = @import("../components/button.zig");
const GameState = @import("../game/state.zig");

pub const Menu = struct {
    pub const Connect = struct {
        pub const bufferSize = 50;
        address: [bufferSize:0]u8 = .{0} ** bufferSize,
        addressPosition: usize = 0,
        connectionStatus: bool = false,
        connect_button: Button.Clickable,
    };
    pub const Login = struct {
        pub const bufferSize = 50;
        username: [bufferSize:0]u8 = .{0} ** bufferSize,
        usernamePosition: usize = 0,
        password: [bufferSize:0]u8 = .{0} ** bufferSize,
        passwordPosition: usize = 0,
        email: [:0]const u8 = "",
        login_button: Button.Clickable,
    };
    pub const Configuration = struct {
        currentScreenMode: enum {
            windowed,
            fullscreen,
        } = .windowed,
    };
    pub const Assets = struct {
        connection: struct {
            connected_icon: rl.Texture2D,
            not_connected_icon: rl.Texture2D,
        },
        music: rl.Music,
        logo: rl.Texture,

        backgrounds: struct {
            main: struct {
                texture: rl.Texture,
                scrolling: f32 = 0,
            },
            character_selection: rl.Texture,
        },
        font: rl.Font,
        sounds: struct {
            buttons: struct {
                select: rl.Sound,
                click: rl.Sound,
            },
            error_sound: rl.Sound,
        },
    };
    pub const Character = struct {
        pub const Creation = struct {
            name_position: usize = 0,
            name: [:0]u8,
        };
        pub const Selection = struct {
            list: []const GameState.World.Character = &.{},
            buttons: Button.SelectableGroup,
            join_world_button: Button.Clickable,
        };
        create: Creation,
        select: Selection,
    };
    pub const Main = struct {
        login_button: Button.Clickable,
        logout_button: Button.Clickable,
        registry_button: Button.Clickable,
        connect_button: Button.Clickable,
        back_button: Button.Clickable.Back,
    };

    login: Login,
    connect: Connect,
    config: Configuration = .{},
    character: Character,
    main: Main,
    assets: *Menu.Assets,
};

fn userRegistryButton(gameState: *GameState) void {
    const buttonSize = Button.Sizes.extraLarge(gameState);
    const buttonPosition: rl.Vector2 = .{
        .x = (gameState.width / 2) - (buttonSize.x / 2),
        .y = (gameState.height / 2) - (buttonSize.y / 2),
    };
    var registry_button = &gameState.menu.main.registry_button;
    if (registry_button.at(
        "Create User",
        buttonPosition,
        buttonSize,
        config.ColorPalette.primary,
    )) gameState.scene = .user_registry;
}

fn userLoginButton(gameState: *GameState) !void {
    const buttonSize = Button.Sizes.extraLarge(gameState);
    const createUserButtonX = (gameState.width / 2) - (buttonSize.x / 2);
    const createUserButtonY = (gameState.height / 2) - (buttonSize.y / 2);
    const buttonPosition: rl.Vector2 = .{
        .x = createUserButtonX,
        .y = createUserButtonY + buttonSize.y + config.menuButtonsPadding,
    };

    const label, const next_scene: GameState.Scene = if (gameState.connection.handler == null) .{ "Login", .user_login } else .{ "Select Character", .character_selection };
    const loginButton = &gameState.menu.main.login_button;
    loginButton.disabled = !gameState.connection.is_connected;
    if (loginButton.at(
        label,
        buttonPosition,
        buttonSize,
        config.ColorPalette.primary,
    )) {
        if (next_scene == .character_selection) try server.user.getCharacters(gameState);
        gameState.scene = next_scene;
    }
}

pub fn userLogoutButton(gameState: *GameState) void {
    _ = gameState.connection.handler orelse return;

    const buttonSize = Button.Sizes.tiny(gameState);
    const buttonPosition: rl.Vector2 = .{
        .x = gameState.width - buttonSize.x - 3 * config.menuButtonsPadding,
        .y = gameState.height - buttonSize.y - 3 * config.menuButtonsPadding,
    };

    const logoutButton = gameState.menu.main.logout_button;
    if (logoutButton.at(
        "Logout",
        buttonPosition,
        buttonSize,
        config.ColorPalette.primary,
    )) {
        server.user.logout(gameState);
    }
}

fn userConnectButton(gameState: *GameState) void {
    const buttonSize = Button.Sizes.extraLarge(gameState);
    const createUserButtonX = (gameState.width / 2) - (buttonSize.x / 2);
    const createUserButtonY = (gameState.height / 2) - (buttonSize.y / 2);
    const buttonPosition: rl.Vector2 = .{
        .x = createUserButtonX,
        .y = createUserButtonY + 2 * buttonSize.y + 2 * config.menuButtonsPadding,
    };
    const label, const next_scene: GameState.Scene = if (gameState.connection.is_connected) .{ "Disconnect", .nothing } else .{ "Connect", .connect };
    const connect_button = &gameState.menu.main.connect_button;
    if (connect_button.at(
        label,
        buttonPosition,
        buttonSize,
        config.ColorPalette.primary,
    )) {
        if (next_scene == .nothing) {
            server.user.logout(gameState);
            std.posix.close(gameState.connection.node.fd);
            gameState.connection.is_connected = false;
        }
        gameState.scene = next_scene;
    }
}

pub fn spawn(gameState: *GameState) !void {
    userRegistryButton(gameState);
    try userLoginButton(gameState);
    userConnectButton(gameState);
    userLogoutButton(gameState);
}

pub fn drawBackgroundColor(width: f32, height: f32, position: rl.Vector2) void {
    const border = 10;
    const bannerSize: rl.Vector2 = .{
        .x = width + 2 * border,
        .y = height + 2 * border,
    };
    const bannerPosition: rl.Vector2 = .{
        .x = position.x - border,
        .y = position.y - border,
    };
    rl.drawRectangleV(bannerPosition, bannerSize, config.ColorPalette.background);
}

pub fn displayLogo(gameState: *GameState) void {
    const texture = gameState.menu.assets.logo;
    const width: f32 = @floatFromInt(texture.width);
    const height: f32 = @floatFromInt(texture.height);
    const position: rl.Vector2 = .{
        .x = gameState.width / 2 - width / 2,
        .y = gameState.height / 20,
    };
    drawBackgroundColor(width, height, position);
    rl.drawTextureEx(texture, position, 0.0, 1, rl.Color.white);
}

pub fn manageBackground(gameState: *GameState) void {
    const scrolling = &gameState.menu.assets.backgrounds.main.scrolling;
    const background = gameState.menu.assets.backgrounds.main.texture;
    const fWidth: f32 = @floatFromInt(background.width);
    scrolling.* -= 0.25;
    if (scrolling.* <= -fWidth * 2) scrolling.* = 0;

    rl.drawTextureEx(background, .{ .x = scrolling.*, .y = 0 }, 0, 1, rl.Color.white);
    rl.drawTextureEx(background, .{ .x = fWidth + scrolling.*, .y = 0 }, 0, 1, rl.Color.white);
}

pub fn loadAssets() !Menu.Assets {
    return .{ .connection = .{
        .not_connected_icon = try assets.texture(config.assets.paths.menu.connection.notConnected),
        .connected_icon = try assets.texture(config.assets.paths.menu.connection.connected),
    }, .music = try assets.music(config.assets.paths.menu.music.background), .logo = try assets.texture(config.assets.paths.menu.logo), .backgrounds = .{
        .main = .{ .texture = try assets.texture(config.assets.paths.menu.background.main) },
        .character_selection = try assets.texture(config.assets.paths.menu.background.character_selection),
    }, .font = try assets.font(config.assets.paths.menu.font), .sounds = .{
        .buttons = .{
            .select = try assets.sound(config.assets.paths.menu.sounds.buttons.select),
            .click = try assets.sound(config.assets.paths.menu.sounds.buttons.click),
        },
        .error_sound = try assets.sound(config.assets.paths.menu.sounds.error_sound),
    } };
}
