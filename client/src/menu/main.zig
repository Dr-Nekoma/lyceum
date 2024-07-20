const rl = @import("raylib");
const config = @import("../config.zig");
const button = @import("../components/button.zig");
const GameState = @import("../game/state.zig");

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

pub fn stdButtonSize(gameState: *const GameState) rl.Vector2 {
    return .{
        .x = gameState.width / 4,
        .y = gameState.height / 8,
    };
}

fn userRegistryButton(gameState: *GameState) void {
    const buttonSize = stdButtonSize(gameState);
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

fn userLoginButton(gameState: *GameState) void {
    const buttonSize = stdButtonSize(gameState);
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

pub fn spawn(gameState: *GameState) void {
    userRegistryButton(gameState);
    userLoginButton(gameState);
}
