const animate = @import("animation.zig");
const camera = @import("camera.zig");
const physics = @import("physics.zig");
const rl = @import("raylib");
const server = @import("../server/main.zig");
const GameState = @import("state.zig");
const std = @import("std");

fn drawPlayers(gameState: *GameState) void {
    var player_iterator = gameState.world.other_players.valueIterator();
    while (player_iterator.next()) |player| {
        physics.character.draw(player, player.stats.face_direction);
        animate.character.update(player);
    }
}

fn controlInput(entity: *GameState.World.Character) u16 {
    var tempAngle = entity.stats.face_direction;
    const velocity = &entity.velocity;
    const deltaTime = rl.getFrameTime();
    const deltaVelocity = deltaTime * physics.character.acceleration;

    if (entity.inventory.hud.chat.mode == .writing) return tempAngle;

    if (rl.isKeyDown(.key_d)) {
        velocity.z -= deltaVelocity;
        tempAngle = 180;
    } else if (rl.isKeyDown(.key_a)) {
        velocity.z += deltaVelocity;
        tempAngle = 0;
    } else if (rl.isKeyDown(.key_w)) {
        velocity.x -= deltaVelocity;
        tempAngle = 270;
    } else if (rl.isKeyDown(.key_s)) {
        velocity.x += deltaVelocity;
        tempAngle = 90;
    }

    return tempAngle;
}

pub fn spawn(gameState: *GameState) !void {
    rl.beginMode3D(gameState.world.camera);
    defer rl.endMode3D();

    const tempAngle = controlInput(&gameState.world.character);
    physics.character.draw(&gameState.world.character, tempAngle);
    animate.character.update(&gameState.world.character);
    camera.update(gameState);
    try server.character.update(gameState);

    drawPlayers(gameState);

    rl.drawGrid(2000, 10.0);

    if (rl.isKeyDown(.key_backslash)) {
        try server.character.exitMap(gameState);
        rl.enableCursor();
    }
}
