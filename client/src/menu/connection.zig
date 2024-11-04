const config = @import("../config.zig");
const connection = @import("../components/connection_status.zig");
const menu = @import("main.zig");
const messages = @import("../server/messages.zig");
const rl = @import("raylib");
const std = @import("std");
const text = @import("../components/text.zig");
const zerl = @import("zerl");
const Button = @import("../components/button.zig");
const GameState = @import("../game/state.zig");

fn print_connect_server_error(message: anytype) !void {
    const stderr = std.io.getStdErr().writer();
    try stderr.print(
        "Could not connect to Lyceum Server!\n\u{1b}[31mError: \u{1b}[37m{}\n",
        .{message},
    );
}

pub fn connect(gameState: *GameState) !void {
    const buttonSize = Button.Sizes.medium(gameState);
    const serverInfoBoxPosition: rl.Vector2 = .{
        .x = gameState.width / 2,
        .y = (gameState.height / 2) - (buttonSize.y / 2),
    };
    const serverInfoLabel = "Server Address";
    const serverInfoLabelSize: f32 = rl.measureTextEx(gameState.menu.assets.font, serverInfoLabel, config.titleFontSize, config.textSpacing).x;

    const serverInfoLabelPositionX =
        (gameState.width / 2) - (serverInfoLabelSize / 2);
    const serverInfoLabelPositionY =
        serverInfoBoxPosition.y - config.buttonFontSize - 3 * config.menuButtonsPadding;

    const position: rl.Vector2 = .{ .x = serverInfoLabelPositionX, .y = serverInfoLabelPositionY };
    menu.drawBackgroundColor(serverInfoLabelSize, config.titleFontSize, position);
    rl.drawTextEx(
        gameState.menu.assets.font,
        serverInfoLabel,
        position,
        config.titleFontSize,
        config.textSpacing,
        rl.Color.white,
    );
    const serverInfoText = text{
        .content = &gameState.menu.connect.address,
        .position = &gameState.menu.connect.addressPosition,
    };
    serverInfoText.at(serverInfoBoxPosition, text.menuTextBoxSize, &gameState.menu.assets.font);

    const buttonPosition: rl.Vector2 = .{
        .x = (gameState.width / 2) - (buttonSize.x / 2),
        .y = serverInfoBoxPosition.y + text.menuTextBoxSize.y + 5 * config.menuButtonsPadding,
    };

    var connectButton = &gameState.menu.connect.connect_button;
    connectButton.disabled = !(serverInfoText.position.* > 0);
    if (connectButton.at(
        "Connect",
        buttonPosition,
        buttonSize,
        config.ColorPalette.primary,
    )) {
        const node_status = zerl.ei.ei_init();
        if (node_status != 0) {
            gameState.errorElem.update(.node_status);
            return;
        }
        const node = gameState.connection.node;
        zerl.establish_connection(
            node,
            GameState.Connection.process_name,
            gameState.menu.connect.address[0..gameState.menu.connect.addressPosition],
        ) catch |error_value| {
            try print_connect_server_error(error_value);
            gameState.errorElem.update(.node_connection);
            return;
        };
        gameState.scene = .nothing;
        gameState.connection.is_connected = true;
    }
}

pub fn status(gameState: *GameState) void {
    const statusWidget = connection{
        .icons = .{
            .connected = &gameState.menu.assets.connection.connected_icon,
            .not_connected = &gameState.menu.assets.connection.not_connected_icon,
        },
    };
    statusWidget.at(gameState.connection.is_connected, gameState.height, &gameState.menu.assets.font);
}
