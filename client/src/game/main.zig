const animate = @import("animation.zig");
const camera = @import("camera.zig");
const config = @import("../config.zig");
const messages = @import("../server/messages.zig");
const physics = @import("physics.zig");
const rl = @import("raylib");
const rm = rl.math;
const server = @import("../server/main.zig");
const std = @import("std");
const GameState = @import("state.zig");

fn drawPlayers(gameState: *GameState) void {
    var player_iterator = gameState.world.other_players.valueIterator();
    while (player_iterator.next()) |player| {
        physics.character.draw(player, &gameState.world.map, player.stats.face_direction);
        animate.character.update(player);
    }
}

fn controlInput(entity: *GameState.World.Character) u16 {
    var tempAngle = entity.stats.face_direction;
    const velocity = &entity.velocity;
    const deltaTime = rl.getFrameTime();
    const deltaVelocity = deltaTime * physics.character.acceleration;

    if (entity.inventory.hud.chat.mode == .writing) return tempAngle;

    if (rl.isKeyDown(.key_w)) {
        velocity.z -= deltaVelocity;
        tempAngle = 180;
    } else if (rl.isKeyDown(.key_s)) {
        velocity.z += deltaVelocity;
        tempAngle = 0;
    } else if (rl.isKeyDown(.key_a)) {
        velocity.x -= deltaVelocity;
        tempAngle = 270;
    } else if (rl.isKeyDown(.key_d)) {
        velocity.x += deltaVelocity;
        tempAngle = 90;
    }

    return tempAngle;
}

fn drawWorld(player: *const GameState.World.Character, world: *const GameState.World.Map) !void {
    const height: i32 = @intCast(world.instance.height);
    const width: i32 = @intCast(world.instance.width);
    const tiles = world.instance.tiles;

    const x_tile: i32 = @intFromFloat(player.position.x / config.assets.tile.size);
    const y_tile: i32 = @intFromFloat(player.position.z / config.assets.tile.size);

    const widthBoundaries: struct { usize, usize } = .{
        @intCast(std.math.clamp(x_tile - config.fov, 0, width)),
        @intCast(std.math.clamp(x_tile + config.fov, 0, width)),
    };
    const heightBoundaries: struct { usize, usize } = .{
        @intCast(std.math.clamp(y_tile - config.fov, 0, height)),
        @intCast(std.math.clamp(y_tile + config.fov, 0, height)),
    };
    for (heightBoundaries.@"0"..(heightBoundaries.@"1")) |y| {
        for (widthBoundaries.@"0"..(widthBoundaries.@"1")) |x| {
            const tile = tiles[world.instance.width * y + x];
            if (world.tiles.get(tile)) |tileData| {
                const tileModel, _ = tileData;
                const fy: f32 = @floatFromInt(y);
                const fx: f32 = @floatFromInt(x);
                const position: rl.Vector3 = .{
                    .x = (fx * config.assets.tile.size) + config.assets.tile.size / 2,
                    .y = config.assets.tile.level,
                    .z = (fy * config.assets.tile.size) + config.assets.tile.size / 2,
                };
                rl.drawModelEx(tileModel.?, position, config.assets.tile.axis, config.assets.tile.angle, config.assets.tile.scale, rl.Color.white);
            } else {
                std.debug.print("[ERROR] Tile kind not present in asset pool: .{}\n", .{tile});
                return error.tile_kind_not_found;
            }
        }
    }
    // TODO: Implement same loop but for objects
}

pub fn spawn(gameState: *GameState) !void {
    rl.beginMode3D(gameState.world.camera);
    defer rl.endMode3D();

    drawPlayers(gameState);

    const tempAngle = controlInput(&gameState.world.character);
    physics.character.draw(&gameState.world.character, &gameState.world.map, tempAngle);
    animate.character.update(&gameState.world.character);
    camera.update(gameState);
    try server.character.update(gameState);

    rl.drawGrid(2000, 10.0);

    if (rl.isKeyDown(.key_backslash)) {
        try server.character.exitMap(gameState);
        rl.enableCursor();
    }

    try drawWorld(&gameState.world.character, &gameState.world.map);
}
