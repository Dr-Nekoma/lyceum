const erl = @import("../erlang.zig");
const messages = @import("../server_messages.zig");
const rl = @import("raylib");
const config = @import("../config.zig");
const Button = @import("../components/button.zig");
const text = @import("../components/text.zig");
const connection = @import("../components/connection_status.zig");
const GameState = @import("../game/state.zig");
const menu = @import("main.zig");
const std = @import("std");

pub fn connect(gameState: *GameState) !void {
    const buttonSize = Button.Sizes.medium(gameState);
    const serverInfoBoxPosition: rl.Vector2 = .{
        .x = gameState.width / 2,
        .y = (gameState.height / 2) - (buttonSize.y / 2),
    };
    const serverInfoLabel = "Server IP";
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
        .content = &gameState.menu.server.ip,
        .position = &gameState.menu.server.ipPosition,
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
        const connection_status = erl.ei.ei_init();
        const node = gameState.connection.node;
        if (connection_status != 0) return error.ei_init_failed;
        erl.establish_connection(node, gameState.menu.server.ip[0..gameState.menu.server.ipPosition]) catch |error_value| {
            try erl.print_connect_server_error(error_value);
            std.process.exit(2);
        };
        gameState.connection.is_connected = true;
        // // TODO: Remove this from here and go to the login screen
        // // For now, everyone will be Magueta
        // if (gameState.node) |nod| {
        //     try messages.send_with_self(nod, .{
        //         .login = .{
        //             .username = "mmagueta",
        //             .password = "password123",
        //         },
        //     });
        //     std.mem.copyForwards(u8, gameState.menu.login.username[0..8], "mmagueta");
        //     gameState.menu.login.usernamePosition = 8;
        // }
        // if (gameState.node) |nod| {
        //     nod.handler, gameState.menu.email = try messages.receive_login_response(gameState.allocator, nod);
        // }
        // // std.debug.print("We received stuff when connection: {?} | {s}\n", .{ gameState.node.handler, gameState.menu.email });
        // gameState.scene = .join;
        gameState.scene = .nothing;
        // gameState.scene = .join;
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
