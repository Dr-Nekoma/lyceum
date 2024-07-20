const messages = @import("../server_messages.zig");
const std = @import("std");
const rl = @import("raylib");
const config = @import("../config.zig");
const attribute = @import("../components/attribute.zig");
const button = @import("../components/button.zig");
const text = @import("../components/text.zig");
const assets = @import("../assets.zig");
const GameState = @import("../game/state.zig");

fn stdButtonSize(gameState: *const GameState) rl.Vector2 {
    return .{
        .x = gameState.width / 4,
        .y = gameState.height / 10,
    };
}

// TODO: add limit for total number of points when creating a character
// TODO: add create button available to click when character is valid
fn emptyCharacter(gameState: *GameState) !void {
    var currentTextPosition: rl.Vector2 = .{
        .x = 50,
        .y = 150,
    };
    const textSize: rl.Vector2 = .{
        .x = 25,
        .y = 25,
    };
    const fieldPadding = 25;
    inline for (std.meta.fields(messages.Erlang_Character)) |field| {
        if (comptime isDifferent(field.name, &.{ "name", "map_name", "x_position", "y_position" })) {
            var mutable_name: [:0]u8 = try gameState.allocator.allocSentinel(u8, field.name.len, 0);
            std.mem.copyForwards(u8, mutable_name, field.name);
            mutable_name[0] = std.ascii.toUpper(mutable_name[0]);
            const attributeComp = attribute{
                .current = &@field(gameState.current_character, field.name),
                .text = mutable_name,
                .textPosition = .{
                    .x = currentTextPosition.x,
                    .y = currentTextPosition.y,
                },
                .textSize = textSize,
                .textColor = rl.Color.white,
            };
            try attributeComp.at();
            currentTextPosition.y += textSize.y + fieldPadding;
        } else if (std.mem.eql(u8, field.name, "name")) {
            const nameBoxPosition: rl.Vector2 = .{
                .x = 50,
                .y = 50,
            };
            const nameLabelPositionY =
                nameBoxPosition.y - config.buttonFontSize - 2 * config.menuButtonsPadding;

            rl.drawText(
                "Name:",
                @intFromFloat(nameBoxPosition.x),
                @intFromFloat(nameLabelPositionY),
                config.buttonFontSize,
                rl.Color.white,
            );
            const nameText = text{
                .content = gameState.menu.character_name,
                .position = &gameState.test_value,
            };
            nameText.at(nameBoxPosition);
            gameState.current_character.name = gameState.menu.character_name;
        } else {
            std.debug.print("Not editable: .{s}\n", .{field.name});
        }
    }
}

fn isDifferent(string: [:0]const u8, forbiddens: []const [:0]const u8) bool {
    for (forbiddens) |forbidden| {
        if (std.mem.eql(u8, string, forbidden)) {
            return false;
        }
    }
    return true;
}

pub fn selection(gameState: *GameState) !void {
    const buttonSize = stdButtonSize(gameState);
    const characterButtonY = (gameState.height / 10) - (buttonSize.y / 2);
    var buttonPosition: rl.Vector2 = .{
        .x = buttonSize.x / 4.0,
        .y = characterButtonY,
    };

    var texturePosition: rl.Vector2 = .{
        .x = buttonPosition.x + buttonSize.x / 2 - 150,
        .y = buttonPosition.y + buttonSize.y * 1.5,
    };

    if (gameState.character_list.len != 0) {
        // TODO: Make pagination for 3 characters at a time
        for (gameState.character_list) |character| {
            if (button.at(
                character.character_data.name,
                buttonPosition,
                buttonSize,
                config.ColorPalette.primary,
            )) {
                gameState.current_character = character.character_data;
                gameState.scene = .nothing;
                break;
            }

            rl.drawTextureEx(character.equipment_data, texturePosition, 0.0, 1, rl.Color.white);

            buttonPosition.x += 5.0 * buttonSize.x / 4.0;
            texturePosition.x += 5.0 * buttonSize.x / 4.0;
        }
    } else {
        // std.debug.print("There are no characters for this user bruh xD", .{});
        try emptyCharacter(gameState);
    }
}

pub fn join(gameState: *GameState) !void {
    const msg = try messages.receive_login_response(gameState.allocator, gameState.node);
    switch (msg) {
        .ok => |email| {
            gameState.menu.email = email;
            try messages.send_payload(gameState.node, .{
                .character_list = .{
                    .username = &gameState.menu.login.username,
                    .email = gameState.menu.email,
                },
            });
            std.debug.print("We asked for characters", .{});
            const maybe_characters = try messages.receive_characters_list(gameState.allocator, gameState.node);
            switch (maybe_characters) {
                .ok => |erlang_characters| {

                    // TODO: Discover how to make this work
                    // const teapotEmbed = @embedFile("../assets/teapot.png");
                    // const teapotLoaded = rl.loadImageFromMemory(".png", teapotEmbed, teapotEmbed.len);

                    const teapotImage = try assets.image("teapot.png");

                    var characters = std.ArrayList(messages.Character).init(gameState.allocator);

                    for (erlang_characters) |character| {
                        try characters.append(.{
                            .character_data = character,
                            .equipment_data = rl.loadTextureFromImage(teapotImage),
                        });
                    }

                    gameState.character_list = characters.items;
                    gameState.scene = .character_selection;
                },
                .@"error" => |error_msg| {
                    std.debug.print("ERROR IN SERVER: {s}", .{error_msg});
                    gameState.scene = .nothing;
                },
            }
        },
        .@"error" => |error_msg| {
            std.debug.print("ERROR IN SERVER: {s}", .{error_msg});
            gameState.scene = .nothing;
        },
    }
}
