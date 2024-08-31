const messages = @import("../server_messages.zig");
const rl = @import("raylib");
const config = @import("../config.zig");
const Button = @import("../components/button.zig");
const text = @import("../components/text.zig");
const GameState = @import("../game/state.zig");
const menu = @import("main.zig");
const std = @import("std");

pub fn login(gameState: *GameState) !void {
    const buttonSize = Button.Sizes.medium(gameState);
    const usernameBoxPosition: rl.Vector2 = .{
        .x = gameState.width / 2,
        .y = (gameState.height / 2) - (buttonSize.y / 2),
    };
    const usernameLabel = "User Name";
    const usernameLabelSize: f32 = @floatFromInt(rl.measureText(usernameLabel, config.textFontSize));

    const usernameLabelPositionX =
        (gameState.width / 2) - (usernameLabelSize / 2);
    const usernameLabelPositionY =
        usernameBoxPosition.y - config.buttonFontSize - 2 * config.menuButtonsPadding;

    rl.drawText(
        usernameLabel,
        @intFromFloat(usernameLabelPositionX),
        @intFromFloat(usernameLabelPositionY),
        config.textFontSize,
        rl.Color.white,
    );
    const usernameText = text{
        .content = &gameState.menu.login.username,
        .position = &gameState.menu.login.usernamePosition,
    };
    usernameText.at(usernameBoxPosition);

    const passwordLabel = "Password";
    const passwordLabelSize: f32 = @floatFromInt(rl.measureText(passwordLabel, config.textFontSize));

    const passwordLabelPositionX =
        (gameState.width / 2) - (passwordLabelSize / 2);
    const passwordLabelPositionY =
        usernameBoxPosition.y + text.textBoxSize.y + 2 * config.menuButtonsPadding;

    const passwordBoxPosition: rl.Vector2 = .{
        .x = gameState.width / 2,
        .y = passwordLabelPositionY + config.buttonFontSize + 2 * config.menuButtonsPadding,
    };
    rl.drawText(
        passwordLabel,
        @intFromFloat(passwordLabelPositionX),
        @intFromFloat(passwordLabelPositionY),
        config.textFontSize,
        rl.Color.white,
    );
    const passwordText = text{
        .content = &gameState.menu.login.password,
        .position = &gameState.menu.login.passwordPosition,
    };
    passwordText.at(passwordBoxPosition);

    const buttonPosition: rl.Vector2 = .{
        .x = (gameState.width / 2) - (buttonSize.x / 2),
        .y = passwordBoxPosition.y + text.textBoxSize.y + 5 * config.menuButtonsPadding,
    };
    const loginButton = Button.Clickable{
        .disabled = !(passwordText.position.* > 0 and usernameText.position.* > 0),
    };
    if (loginButton.at(
        "Login",
        buttonPosition,
        buttonSize,
        config.ColorPalette.primary,
    )) {
        // TODO: Add loading animation to wait for response
        // TODO: Add a timeout for login
        std.debug.print("Handler: {?}\n", .{gameState.connection.handler});
        try gameState.send_with_self(.{
            .login = .{
                .username = gameState.menu.login.username[0..gameState.menu.login.usernamePosition],
                .password = gameState.menu.login.password[0..gameState.menu.login.passwordPosition],
            },
        });
        std.debug.print("We are about to receive stuff\n", .{});
        gameState.connection.handler, gameState.menu.email =
            try messages.receive_login_response(gameState.allocator, gameState.connection.node);
        // std.debug.print("We received stuff: {?} | {s}\n", .{ gameState.node.handler, gameState.menu.email });
        gameState.scene = .join;
    }
}
