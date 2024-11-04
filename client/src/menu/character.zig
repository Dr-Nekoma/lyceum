const assets = @import("../assets.zig");
const attribute = @import("../components/attribute.zig");
const config = @import("../config.zig");
const mainMenu = @import("main.zig");
const messages = @import("../server/messages.zig");
const rl = @import("raylib");
const server = @import("../server/main.zig");
const std = @import("std");
const text = @import("../components/text.zig");
const Button = @import("../components/button.zig");
const GameState = @import("../game/state.zig");

pub fn goToSpawn(gameState: *GameState) !void {
    const character = &gameState.world.character;
    character.animation.frames = assets.animations(config.assets.paths.game.character.walker) catch {
        gameState.errorElem.update(.loading_assets);
        return;
    };
    character.model = assets.model(config.assets.paths.game.character.walker) catch {
        gameState.errorElem.update(.loading_assets);
        return;
    };

    try server.character.joinMap(gameState);
    rl.disableCursor();
}

// TODO: add limit for total number of points when creating a character
// TODO: add create button available to click when character is valid
fn emptyCharacter(gameState: *GameState) !void {
    mainMenu.userLogoutButton(gameState);
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
        // TODO: Implement presets instead of allowing the user to change
        // the attributes
        if (comptime isDifferent(field.name, &.{
            "level",
            "health",
            "health_max",
            "mana",
            "mana_max",
            "name",
            "map_name",
            "x_position",
            "y_position",
            "x_velocity",
            "y_velocity",
            "face_direction",
            "state_type",
        })) {
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
            try attributeComp.at(&gameState.menu.assets.font);
            currentTextPosition.y += textSize.y + fieldPadding;
        } else if (std.mem.eql(u8, field.name, "name")) {
            const nameBoxPosition: rl.Vector2 = .{
                .x = 150,
                .y = 50,
            };
            const nameLabelPositionY =
                nameBoxPosition.y - config.buttonFontSize - 2 * config.menuButtonsPadding;

            rl.drawTextEx(
                gameState.menu.assets.font,
                "Name:",
                .{ .x = 50, .y = nameLabelPositionY },
                config.buttonFontSize,
                config.textSpacing,
                rl.Color.white,
            );
            const nameText = text{
                .content = gameState.menu.character.create.name,
                .position = &gameState.menu.character.create.name_position,
            };
            nameText.at(nameBoxPosition, text.menuTextBoxSize, &gameState.menu.assets.font);
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
    mainMenu.userLogoutButton(gameState);

    const buttonSize = Button.Sizes.large(gameState);
    const characterButtonY = (gameState.height / 10) - (buttonSize.y / 2);
    var buttonPosition: rl.Vector2 = .{
        .x = buttonSize.x / 4.0,
        .y = characterButtonY,
    };

    const joinButtonSize = Button.Sizes.small(gameState);
    const joinButtonPosition: rl.Vector2 = .{
        .x = (gameState.width / 2) - (joinButtonSize.x / 2),
        .y = gameState.height - joinButtonSize.y - 3 * config.menuButtonsPadding,
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
                &gameState.menu.assets.font,
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
                &gameState.menu.assets.font,
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
