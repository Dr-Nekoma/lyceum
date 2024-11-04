const config = @import("../config.zig");
const menu = @import("main.zig");
const messages = @import("../server/messages.zig");
const rl = @import("raylib");
const server = @import("../server/main.zig");
const std = @import("std");
const text = @import("../components/text.zig");
const Button = @import("../components/button.zig");
const GameState = @import("../game/state.zig");

pub fn login(gameState: *GameState) !void {
    const buttonSize = Button.Sizes.medium(gameState);
    const usernameBoxPosition: rl.Vector2 = .{
        .x = gameState.width / 2,
        .y = (gameState.height / 2) - (buttonSize.y / 2),
    };
    const usernameLabel = "User Name";
    const usernameLabelSize: f32 = rl.measureTextEx(gameState.menu.assets.font, usernameLabel, config.titleFontSize, config.textSpacing).x;

    const usernameLabelPositionX =
        (gameState.width / 2) - (usernameLabelSize / 2);
    const usernameLabelPositionY =
        usernameBoxPosition.y - config.buttonFontSize - 2 * config.menuButtonsPadding;

    rl.drawTextEx(
        gameState.menu.assets.font,
        usernameLabel,
        .{ .x = usernameLabelPositionX, .y = usernameLabelPositionY },
        config.titleFontSize,
        config.textSpacing,
        rl.Color.white,
    );
    const usernameText = text{
        .content = &gameState.menu.credentials.username,
        .position = &gameState.menu.credentials.usernamePosition,
    };
    usernameText.at(usernameBoxPosition, text.menuTextBoxSize, &gameState.menu.assets.font);

    const passwordLabel = "Password";
    const passwordLabelSize: f32 = rl.measureTextEx(gameState.menu.assets.font, passwordLabel, config.titleFontSize, config.textSpacing).x;

    const passwordLabelPositionX =
        (gameState.width / 2) - (passwordLabelSize / 2);
    const passwordLabelPositionY =
        usernameBoxPosition.y + text.menuTextBoxSize.y + 2 * config.menuButtonsPadding;

    const passwordBoxPosition: rl.Vector2 = .{
        .x = gameState.width / 2,
        .y = passwordLabelPositionY + config.buttonFontSize + 2 * config.menuButtonsPadding,
    };
    rl.drawTextEx(
        gameState.menu.assets.font,
        passwordLabel,
        .{ .x = passwordLabelPositionX, .y = passwordLabelPositionY },
        config.titleFontSize,
        config.textSpacing,
        rl.Color.white,
    );
    const passwordText = text{
        .content = &gameState.menu.credentials.password,
        .position = &gameState.menu.credentials.passwordPosition,
    };
    passwordText.at(passwordBoxPosition, text.menuTextBoxSize, &gameState.menu.assets.font);

    const buttonPosition: rl.Vector2 = .{
        .x = (gameState.width / 2) - (buttonSize.x / 2),
        .y = passwordBoxPosition.y + text.menuTextBoxSize.y + 5 * config.menuButtonsPadding,
    };
    const loginButton = Button.Clickable{
        .disabled = !(passwordText.position.* > 0 and usernameText.position.* > 0),
    };
    if (loginButton.at(
        "Login",
        buttonPosition,
        buttonSize,
        config.ColorPalette.primary,
        &gameState.menu.assets.font,
    )) {
        // TODO: Add loading animation to wait for response
        // TODO: Add a timeout for login
        try server.user.login(gameState);
    }
}
