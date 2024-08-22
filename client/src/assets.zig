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
        .{ .{".glb"}, .{".obj"} },
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
