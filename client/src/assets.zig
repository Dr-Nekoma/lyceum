const rl = @import("raylib");
const std = @import("std");

const base_filepath = "./assets/";

fn checkExtension(extensions: []const [:0]const u8) (fn ([:0]const u8) bool) {
    return struct {
        fn check(filePath: [:0]const u8) bool {
            const fileExtension = filePath[filePath.len - 4 ..];
            for (extensions) |extension| {
                if (std.mem.eql(u8, fileExtension, extension)) {
                    return true;
                }
            }
            return false;
        }
    }.check;
}

fn load(comptime filePath: [:0]const u8, comptime T: type, loader: fn ([:0]const u8) T, predicate: fn ([:0]const u8) bool) !T {
    const allocator: std.mem.Allocator = std.heap.c_allocator;

    var fullFilePath: [:0]u8 = try allocator.allocSentinel(u8, base_filepath.len + filePath.len, 0);
    defer allocator.free(fullFilePath);

    std.mem.copyForwards(u8, fullFilePath[0..], base_filepath);
    std.mem.copyForwards(u8, fullFilePath[base_filepath.len..], filePath);

    if (predicate(fullFilePath)) {
        return loader(fullFilePath);
    } else {
        std.debug.print("Error trying to load file: .{s}", .{fullFilePath});
        return error.could_not_load_asset;
    }
}

pub fn image(comptime imageFilePath: [:0]const u8) !rl.Image {
    return load(imageFilePath, rl.Image, rl.loadImage, checkExtension(&.{ ".png", ".jpg" }));
}

pub fn model(comptime modelFilePath: [:0]const u8) !rl.Model {
    return load(modelFilePath, rl.Model, rl.loadModel, checkExtension(&.{ ".glb", ".obj" }));
}

pub fn texture(comptime textureFilePath: [:0]const u8) !rl.Texture2D {
    return load(textureFilePath, rl.Texture2D, rl.loadTexture, checkExtension(&.{ ".png", "jpg" }));
}
