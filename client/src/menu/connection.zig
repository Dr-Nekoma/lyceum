const config = @import("../config.zig");
const connection = @import("../components/connection_status.zig");
const menu = @import("main.zig");
const messages = @import("../server_messages.zig");
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
    const serverInfoLabelSize: f32 = @floatFromInt(rl.measureText(serverInfoLabel, config.textFontSize));

    const serverInfoLabelPositionX =
        (gameState.width / 2) - (serverInfoLabelSize / 2);
    const serverInfoLabelPositionY =
        serverInfoBoxPosition.y - config.buttonFontSize - 2 * config.menuButtonsPadding;

    rl.drawText(
        serverInfoLabel,
        @intFromFloat(serverInfoLabelPositionX),
        @intFromFloat(serverInfoLabelPositionY),
        config.textFontSize,
        rl.Color.white,
    );
    const serverInfoText = text{
        .content = &gameState.menu.server.address,
        .position = &gameState.menu.server.addressPosition,
    };
    serverInfoText.at(serverInfoBoxPosition);

    const buttonPosition: rl.Vector2 = .{
        .x = (gameState.width / 2) - (buttonSize.x / 2),
        .y = serverInfoBoxPosition.y + text.textBoxSize.y + 5 * config.menuButtonsPadding,
    };
    const connectButton = Button.Clickable{
        .disabled = !(serverInfoText.position.* > 0),
    };
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
        zerl.establish_connection(node, GameState.Connection.process_name, gameState.menu.server.address[0..gameState.menu.server.addressPosition]) catch |error_value| {
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
    statusWidget.at(gameState.connection.is_connected, gameState.height);
}
