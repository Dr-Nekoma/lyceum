const rl = @import("raylib");
const std = @import("std");

const base_filepath = "./assets/";

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
