const config = @import("config.zig");
const map = @import("components/hud/map.zig");
const messages = @import("server/messages.zig");
const rl = @import("raylib");
const rm = rl.math;
const std = @import("std");
const GameState = @import("game/state.zig");
const build_options = @import("build_options");

const base_filepath = build_options.assets;

fn load(
    comptime T: type,
    comptime err: anyerror,
    comptime whitelist: anytype,
    file_path: [:0]const u8,
) !T {
    const valid_extensions = comptime std.StaticStringMap(void).initComptime(whitelist);
    const extension = std.fs.path.extension(file_path);
    if (!valid_extensions.has(extension)) return err;

    const allocator = std.heap.c_allocator;
    const full_path = try std.fs.path.joinZ(allocator, &.{ base_filepath, file_path });
    defer allocator.free(full_path);

    return T.init(full_path);
}

pub fn image(imageFilePath: [:0]const u8) !rl.Image {
    return load(
        rl.Image,
        error.could_not_load_image,
        .{ .{".png"}, .{".jpg"} },
        imageFilePath,
    );
}

pub fn model(modelFilePath: [:0]const u8) !rl.Model {
    return load(
        rl.Model,
        error.could_not_load_model,
        .{ .{".glb"}, .{".obj"}, .{".m3d"} },
        modelFilePath,
    );
}

pub fn texture(textureFilePath: [:0]const u8) !rl.Texture {
    return load(
        rl.Texture,
        error.could_not_load_texture,
        .{ .{".png"}, .{".jpg"} },
        textureFilePath,
    );
}

pub fn font(fontFilePath: [:0]const u8) !rl.Font {
    return load(
        rl.Font,
        error.could_not_load_font,
        .{.{".otf"}},
        fontFilePath,
    );
}

pub fn music(musicFilePath: [:0]const u8) !rl.Music {
    // TODO: figure out the lack of init for this return type
    const allocator: std.mem.Allocator = std.heap.c_allocator;
    const fullFilePath = try std.fs.path.joinZ(allocator, &.{ base_filepath, musicFilePath });
    defer allocator.free(fullFilePath);
    const valid_extensions = comptime std.StaticStringMap(void).initComptime(.{.{ ".ogg", ".wav" }});
    const extension = std.fs.path.extension(fullFilePath);
    if (!valid_extensions.has(extension)) {
        std.debug.print("Error trying to load music: .{s}", .{fullFilePath});
        return error.could_not_load_music;
    }
    return rl.loadMusicStream(fullFilePath);
}

pub fn sound(soundFilePath: [:0]const u8) !rl.Sound {
    // TODO: figure out the lack of init for this return type
    const allocator: std.mem.Allocator = std.heap.c_allocator;
    const fullFilePath = try std.fs.path.joinZ(allocator, &.{ base_filepath, soundFilePath });
    defer allocator.free(fullFilePath);
    const valid_extensions = comptime std.StaticStringMap(void).initComptime(.{.{".ogg"}});
    const extension = std.fs.path.extension(fullFilePath);
    if (!valid_extensions.has(extension)) {
        std.debug.print("Error trying to load sound: .{s}", .{fullFilePath});
        return error.could_not_load_sound;
    }
    return rl.loadSound(fullFilePath);
}

pub fn animations(animationFilePath: [:0]const u8) ![]rl.ModelAnimation {
    // TODO: figure out the lack of init for this return type
    const allocator: std.mem.Allocator = std.heap.c_allocator;
    const fullFilePath = try std.fs.path.joinZ(allocator, &.{ base_filepath, animationFilePath });
    defer allocator.free(fullFilePath);
    const valid_extensions = comptime std.StaticStringMap(void).initComptime(.{.{".m3d"}});
    const extension = std.fs.path.extension(fullFilePath);
    if (!valid_extensions.has(extension)) {
        std.debug.print("Error trying to load animation model: .{s}", .{fullFilePath});
        return error.could_not_load_animation;
    }
    return try rl.loadModelAnimations(fullFilePath);
}

fn resizeImage(imageFilePath: [:0]const u8) !rl.Image {
    var img = try image(imageFilePath);
    img.resizeNN(config.map.mini_map_size, config.map.mini_map_size);
    return img;
}

fn loadTile(kind: messages.Tile) !struct { ?rl.Model, ?rl.Image } {
    return switch (kind) {
        .dirt => .{
            try model(config.assets.paths.game.world.tiles.dirt.model),
            try resizeImage(config.assets.paths.game.world.tiles.dirt.img),
        },
        .grass => .{
            try model(config.assets.paths.game.world.tiles.grass.model),
            try resizeImage(config.assets.paths.game.world.tiles.grass.img),
        },
        .sand => .{
            try model(config.assets.paths.game.world.tiles.sand.model),
            try resizeImage(config.assets.paths.game.world.tiles.sand.img),
        },
        .water => .{
            try model(config.assets.paths.game.world.tiles.water.model),
            try resizeImage(config.assets.paths.game.world.tiles.water.img),
        },
        .empty => unreachable,
    };
}

pub fn tilesTable() !GameState.Tile_Table {
    var initTable = GameState.Tile_Table.initFull(.{ null, null });
    inline for (@typeInfo(messages.Tile).Enum.fields) |field| {
        if (!std.mem.eql(u8, field.name, "empty")) {
            const key: messages.Tile = @enumFromInt(field.value);
            const assets = try loadTile(key);
            initTable.put(key, assets);
        }
    }
    return initTable;
}

pub const Object = struct {
    model: ?rl.Model = null,
    scale: rl.Vector3 = .{ .x = 0, .y = 0, .z = 0 },
    axis: rl.Vector3 = config.assets.object.defaultAxis,
    angle: f32 = config.assets.object.defaultAngle,
};

fn loadObject(kind: messages.Object) !Object {
    return switch (kind) {
        .chest => .{
            .model = try model(config.assets.paths.game.world.objects.chest.model),
            .scale = config.assets.object.chest.scale,
            .axis = config.assets.object.chest.axis,
            .angle = config.assets.object.chest.angle,
        },
        .tree => .{
            .model = try model(config.assets.paths.game.world.objects.tree.model),
            .scale = config.assets.object.tree.scale,
            .axis = config.assets.object.tree.axis,
            .angle = config.assets.object.tree.angle,
        },
        .bush => .{
            .model = try model(config.assets.paths.game.world.objects.bush.model),
            .scale = config.assets.object.bush.scale,
            .axis = config.assets.object.bush.axis,
            .angle = config.assets.object.bush.angle,
        },
        .rock => .{
            // TODO: distinguish rocks from bushes
            .model = try model(config.assets.paths.game.world.objects.rock.model),
            .scale = config.assets.object.rock.scale,
            .axis = config.assets.object.rock.axis,
            .angle = config.assets.object.rock.angle,
        },
        .empty => unreachable,
    };
}

pub fn objectsTable() !GameState.Object_Table {
    var initTable = GameState.Object_Table.initFull(.{});
    inline for (@typeInfo(messages.Object).Enum.fields) |field| {
        if (!std.mem.eql(u8, field.name, "empty")) {
            const key: messages.Object = @enumFromInt(field.value);
            const asset = try loadObject(key);
            initTable.put(key, asset);
        }
    }
    return initTable;
}

const tileRec: rl.Rectangle = .{
    .x = 0,
    .y = 0,
    .width = config.map.mini_map_size,
    .height = config.map.mini_map_size,
};

pub fn createMapImage(world: *const GameState.World.Map) !rl.Image {
    const width = world.instance.width;
    const height = world.instance.height;
    const tiles = world.instance.tiles;
    const miniMapSize: i32 = @intFromFloat(config.map.mini_map_size);
    const iWidth: i32 = @intCast(width * miniMapSize);
    const iHeight: i32 = @intCast(height * miniMapSize);
    var img = rl.genImageColor(iWidth, iHeight, rl.Color.init(0, 0, 0, 255));

    for (0..height) |j| {
        for (0..width) |i| {
            const tile = tiles[width * j + i];
            if (tile != .empty) {
                if (world.tiles.get(tile)) |tileData| {
                    _, const reducedTileImg = tileData;
                    const imgRec: rl.Rectangle = .{
                        .x = @floatFromInt(i * config.map.mini_map_size),
                        .y = @floatFromInt(j * config.map.mini_map_size),
                        .width = config.map.mini_map_size,
                        .height = config.map.mini_map_size,
                    };
                    rl.imageDraw(&img, reducedTileImg.?, tileRec, imgRec, rl.Color.init(0, 0, 0, 255));
                } else {
                    std.debug.print("[ERROR] Tile kind not present in asset pool: .{}\n", .{tile});
                    return error.tile_kind_not_found;
                }
            }
        }
    }
    map.add_borders(&img);
    return img;
}
