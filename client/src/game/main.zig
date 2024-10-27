const animate = @import("animation.zig");
const camera = @import("camera.zig");
const physics = @import("physics.zig");
const rl = @import("raylib");
const server = @import("../server/main.zig");
const GameState = @import("state.zig");

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
    if (rl.isKeyDown(.key_space)) {
        velocity.y += deltaVelocity;
    }
    if (rl.isKeyDown(.key_left_control)) {
        velocity.y -= deltaVelocity;
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

    if (rl.isKeyDown(.key_q)) {
        try server.character.exitMap(gameState);
        rl.enableCursor();
        gameState.scene = .nothing;
    }

    rl.drawGrid(20, 10.0);
}
