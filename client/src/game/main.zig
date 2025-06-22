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
const GameCharacter = @import("character.zig");

fn drawPlayers(gameState: *GameState) void {
    const mainPlayer = &gameState.world.character;
    var player_iterator = gameState.world.other_players.valueIterator();
    while (player_iterator.next()) |player| {
        if (GameState.canDisplayPlayer(mainPlayer, player)) {
            physics.character.draw(player, &gameState.world.map, player.stats.face_direction);
            animate.character.update(player);
        }
    }
}

fn controlInput(gameState: *GameState) !i16 {
    const entity = &gameState.world.character;
    var tempAngle = entity.stats.face_direction;
    const velocity = &entity.velocity;
    const deltaTime = rl.getFrameTime();
    const deltaVelocity = deltaTime * physics.character.acceleration;

    if (entity.inventory.hud.chat.mode == .writing) return tempAngle;

    if (rl.isKeyDown(.w)) {
        velocity.z -= deltaVelocity;
        tempAngle = 180;
    } else if (rl.isKeyDown(.s)) {
        velocity.z += deltaVelocity;
        tempAngle = 0;
    } else if (rl.isKeyDown(.a)) {
        velocity.x -= deltaVelocity;
        tempAngle = 270;
    } else if (rl.isKeyDown(.d)) {
        velocity.x += deltaVelocity;
        tempAngle = 90;
    }

    if (rl.isKeyDown(.backslash)) {
        try server.character.exitMap(gameState);
        rl.enableCursor();
    }

    return tempAngle;
}

fn assetPosition(x: f32, y: f32, floor: f32) rl.Vector3 {
    return .{
        .x = (x * config.assets.tile.size) + config.assets.tile.size / 2,
        .y = floor,
        .z = (y * config.assets.tile.size) + config.assets.tile.size / 2,
    };
}

fn scaleObject(
    position: messages.World.Position,
    default_scale: rl.Vector3,
    world: *const GameState.World.Map,
) rl.Vector3 {
    if (world.resources.get(position)) |resource| {
        const quantity: f32 = @floatFromInt(resource.quantity);
        const capacity: f32 = @floatFromInt(resource.capacity);
        return rl.Vector3.scale(default_scale, quantity / capacity);
    } else {
        return default_scale;
    }
}

fn drawWorld(player: *const GameCharacter, world: *const GameState.World.Map) !void {
    const height: i32 = @intCast(world.instance.height);
    const width: i32 = @intCast(world.instance.width);
    const tiles = world.instance.tiles;
    const objects = world.instance.objects;

    const x_tile: i32 = @intFromFloat(player.position.x / config.assets.tile.size);
    const y_tile: i32 = @intFromFloat(player.position.z / config.assets.tile.size);

    const minWidth: usize, const maxWidth: usize = .{
        @intCast(std.math.clamp(x_tile - config.fov - 1, 0, width)),
        @intCast(std.math.clamp(x_tile + config.fov + 1, 0, width)),
    };
    const minHeight: usize, const maxHeight: usize = .{
        @intCast(std.math.clamp(y_tile - config.fov - 1, 0, height)),
        @intCast(std.math.clamp(y_tile + config.fov + 1, 0, height)),
    };
    for (minHeight..maxHeight) |y| {
        for (minWidth..maxWidth) |x| {
            const tile = tiles[world.instance.width * y + x];
            const object = objects[world.instance.width * y + x];
            const fy: f32 = @floatFromInt(y);
            const fx: f32 = @floatFromInt(x);
            if (world.tiles.get(tile)) |tileData| {
                const tileModel, _ = tileData;
                const position = assetPosition(fx, fy, config.assets.tile.level);
                rl.drawModelEx(tileModel.?, position, config.assets.tile.axis, config.assets.tile.angle, config.assets.tile.scale, config.ColorPalette.secondary);
            } else {
                std.debug.print("[ERROR] Tile kind not present in asset pool: .{}\n", .{tile});
                return error.tile_kind_not_found;
            }
            if (object != .empty) {
                if (world.objects.get(object)) |objectData| {
                    const position = assetPosition(fx, fy, config.assets.object.defaultLevel);
                    rl.drawModelEx(
                        objectData.model.?,
                        position,
                        objectData.axis,
                        objectData.angle,
                        scaleObject(.{ fx, fy }, objectData.scale, world),
                        config.ColorPalette.secondary,
                    );
                }
            }
        }
    }
}

pub fn spawn(gameState: *GameState) !void {
    rl.beginMode3D(gameState.world.camera);
    defer rl.endMode3D();

    const tempAngle = try controlInput(gameState);

    physics.character.draw(&gameState.world.character, &gameState.world.map, tempAngle);
    animate.character.update(&gameState.world.character);
    camera.update(gameState);

    try server.character.update(gameState);

    rl.drawGrid(2000, 10.0);

    drawPlayers(gameState);

    try drawWorld(&gameState.world.character, &gameState.world.map);
}
