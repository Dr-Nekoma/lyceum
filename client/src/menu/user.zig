const messages = @import("../server_messages.zig");
const rl = @import("raylib");
const config = @import("../config.zig");
const button = @import("../components/button.zig");
const text = @import("../components/text.zig");
const GameState = @import("../game/state.zig");
const menu = @import("main.zig");

pub fn login(gameState: *GameState) !void {
    const buttonSize = menu.stdButtonSize(gameState);
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
