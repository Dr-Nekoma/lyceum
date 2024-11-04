const assets = @import("../assets.zig");
const config = @import("../config.zig");
const rl = @import("raylib");
const server = @import("../server/main.zig");
const std = @import("std");
const Button = @import("../components/button.zig");
const Clickable = Button.Clickable{};
const GameState = @import("../game/state.zig");

pub const Menu = struct {
    pub const Server = struct {
        pub const bufferSize = 50;
        address: [bufferSize:0]u8 = .{0} ** bufferSize,
        addressPosition: usize = 0,
        connectionStatus: bool = false,
    };
    pub const Credentials = struct {
        pub const bufferSize = 50;
        pub const logout_button: Button.Clickable = Button.Clickable{};
        username: [bufferSize:0]u8 = .{0} ** bufferSize,
        usernamePosition: usize = 0,
        password: [bufferSize:0]u8 = .{0} ** bufferSize,
        passwordPosition: usize = 0,
        email: [:0]const u8 = "",
        login_button: Button.Clickable = Button.Clickable{ .disabled = true },
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
    };
    pub const Character = struct {
        pub const Creation = struct {
            name_position: usize = 0,
            name: [:0]u8,
        };
        pub const Selection = struct {
            list: []const GameState.World.Character = &.{},
            buttons: Button.SelectableGroup = .{},
            join_world_button: Button.Clickable = Button.Clickable{ .disabled = true },
        };
        create: Creation,
        select: Selection = .{},
    };

    credentials: Credentials = .{},
    server: Server = .{},
    config: Configuration = .{},
    character: Character,
    assets: Assets,
};

fn userRegistryButton(gameState: *GameState) void {
    const buttonSize = Button.Sizes.extraLarge(gameState);
    const buttonPosition: rl.Vector2 = .{
        .x = (gameState.width / 2) - (buttonSize.x / 2),
        .y = (gameState.height / 2) - (buttonSize.y / 2),
    };
    if (Clickable.at(
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
    const loginButton = &gameState.menu.credentials.login_button;
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

    const logoutButton = Menu.Credentials.logout_button;
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
    if (Clickable.at(
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

pub fn loadAssets() !Menu.Assets {
    const notConnected = try assets.texture(config.assets.paths.menu.connection.connected);
    const connected = try assets.texture(config.assets.paths.menu.connection.notConnected);
    return .{
        .connection = .{
            .not_connected_icon = notConnected,
            .connected_icon = connected,
        },
    };
}
