const rl = @import("raylib");
const config = @import("../config.zig");
const Button = @import("../components/button.zig");
const Clickable = Button.Clickable{};
const GameState = @import("../game/state.zig");

pub const Menu = struct {
    pub const Server = struct {
        ip: [bufferSize:0]u8 = .{0} ** bufferSize,
        ipPosition: usize = 0,
        pub const bufferSize = 50;
    };
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
    character_buttons: Button.SelectableGroup = .{},
    join_world_button: Button.Clickable = Button.Clickable{ .disabled = true },
    login: Login = .{},
    server: Server = .{},
    email: [:0]const u8 = "",
    config: Configuration = .{},
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

fn userLoginButton(gameState: *GameState) void {
    const buttonSize = Button.Sizes.extraLarge(gameState);
    const createUserButtonX = (gameState.width / 2) - (buttonSize.x / 2);
    const createUserButtonY = (gameState.height / 2) - (buttonSize.y / 2);
    const buttonPosition: rl.Vector2 = .{
        .x = createUserButtonX,
        .y = createUserButtonY + buttonSize.y + config.menuButtonsPadding,
    };
    if (Clickable.at(
        "Login",
        buttonPosition,
        buttonSize,
        config.ColorPalette.primary,
    )) gameState.scene = .user_login;
}

fn userConnectButton(gameState: *GameState) void {
    const buttonSize = Button.Sizes.extraLarge(gameState);
    const createUserButtonX = (gameState.width / 2) - (buttonSize.x / 2);
    const createUserButtonY = (gameState.height / 2) - (buttonSize.y / 2);
    const buttonPosition: rl.Vector2 = .{
        .x = createUserButtonX,
        .y = createUserButtonY + 2 * buttonSize.y + 2 * config.menuButtonsPadding,
    };
    if (Clickable.at(
        "Connect",
        buttonPosition,
        buttonSize,
        config.ColorPalette.primary,
    )) gameState.scene = .connect;
}

pub fn spawn(gameState: *GameState) void {
    userRegistryButton(gameState);
    userLoginButton(gameState);
    userConnectButton(gameState);
}
