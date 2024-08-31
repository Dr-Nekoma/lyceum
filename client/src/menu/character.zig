const assets = @import("../assets.zig");
const attribute = @import("../components/attribute.zig");
const config = @import("../config.zig");
const messages = @import("../server_messages.zig");
const protocol = @import("../game/protocol.zig");
const rl = @import("raylib");
const std = @import("std");
const text = @import("../components/text.zig");
const Button = @import("../components/button.zig");
const GameState = @import("../game/state.zig");

pub fn goToSpawn(gameState: *GameState) !void {
    // Source: https://free3d.com/3d-model/knight-low-poly-542752.html
    const model = try assets.model("knight.glb");
    gameState.world.character.model = model;
    rl.disableCursor();
    try protocol.pingJoinMap(gameState);
    gameState.scene = .spawn;
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
    inline for (std.meta.fields(messages.Character_Info)) |field| {
        if (comptime isDifferent(field.name, &.{ "name", "map_name", "x_position", "y_position", "face_direction" })) {
            const mutable_name: [:0]u8 = try gameState.allocator.allocSentinel(u8, field.name.len, 0);
            std.mem.copyForwards(u8, mutable_name, field.name);
            mutable_name[0] = std.ascii.toUpper(mutable_name[0]);
            const attributeComp = attribute{
                .current = &@field(gameState.world.character.stats, field.name),
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
                .x = 150,
                .y = 50,
            };
            const nameLabelPositionY =
                nameBoxPosition.y - config.buttonFontSize - 2 * config.menuButtonsPadding;

            rl.drawText(
                "Name:",
                50,
                @intFromFloat(nameLabelPositionY),
                config.buttonFontSize,
                rl.Color.white,
            );
            const nameText = text{
                .content = gameState.menu.character.create.name,
                .position = &gameState.menu.character.create.name_position,
            };
            nameText.at(nameBoxPosition);
            gameState.world.character.stats.name = gameState.menu.character.create.name;
        } else {
            // std.debug.print("Not editable: .{s}\n", .{field.name});
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
    const buttonSize = Button.Sizes.large(gameState);
    const characterButtonY = (gameState.height / 10) - (buttonSize.y / 2);
    var buttonPosition: rl.Vector2 = .{
        .x = buttonSize.x / 4.0,
        .y = characterButtonY,
    };

    const joinButtonSize = Button.Sizes.medium(gameState);
    const joinButtonPosition: rl.Vector2 = .{
        .x = (gameState.width / 2) - (joinButtonSize.x / 2),
        .y = (gameState.height / 2) - (joinButtonSize.y / 2) + (gameState.height / 3),
    };

    var texturePosition: rl.Vector2 = .{
        .x = buttonPosition.x + buttonSize.x / 2 - 150,
        .y = buttonPosition.y + buttonSize.y * 1.5,
    };

    if (gameState.menu.character.select.list.len != 0) {
        // TODO: Make pagination for 3 characters at a time
        const currentSelected = &gameState.menu.character.select.buttons.selected;
        const joinButton = &gameState.menu.character.select.join_world_button;
        for (0.., gameState.menu.character.select.list) |index, character| {
            var currentButton = gameState.menu.character.select.buttons.instances[index];
            currentButton.index = index;
            if (Button.Selectable.at(
                &currentButton,
                character.stats.name,
                buttonPosition,
                buttonSize,
                config.ColorPalette.primary,
                currentSelected.*,
            )) {
                currentSelected.* = index;
                gameState.world.character.stats = character.stats;
            }

            if (character.preview) |preview| {
                rl.drawTextureEx(preview, texturePosition, 0.0, 1, rl.Color.white);
            }
            joinButton.disabled = currentSelected.* == null;
            if (joinButton.at(
                "Join Map",
                joinButtonPosition,
                joinButtonSize,
                config.ColorPalette.primary,
            )) {
                try goToSpawn(gameState);
            }

            buttonPosition.x += 5.0 * buttonSize.x / 4.0;
            texturePosition.x += 5.0 * buttonSize.x / 4.0;
        }
    } else {
        // std.debug.print("There are no characters for this user bruh xD", .{});
        try emptyCharacter(gameState);
    }
}

pub fn join(gameState: *GameState) !void {
    const node = gameState.connection.node;
    try node.send(messages.Payload{
        .list_characters = .{
            .username = gameState.menu.credentials.username[0..gameState.menu.credentials.usernamePosition],
            .email = gameState.menu.credentials.email,
        },
    }, gameState.connection.handler);

    const maybe_characters = try messages.receive_characters_list(gameState.allocator, node);
    switch (maybe_characters) {
        .ok => |erlang_characters| {
            // todo: discover how to make this work
            // const teapotembed = @embedfile("../assets/teapot.png");
            // const teapotloaded = rl.loadimagefrommemory(".png", teapotembed, teapotembed.len);

            const teapot = try assets.texture("teapot.png");

            var characters = std.ArrayList(GameState.World.Character).init(gameState.allocator);

            for (erlang_characters) |stats| {
                try characters.append(.{
                    .stats = stats,
                    .preview = teapot,
                });
            }

            gameState.menu.character.select.list = characters.items;
            gameState.scene = .character_selection;
        },
        .@"error" => |error_msg| {
            std.debug.print("error in server: {s}", .{error_msg});
            gameState.scene = .nothing;
        },
    }
}
