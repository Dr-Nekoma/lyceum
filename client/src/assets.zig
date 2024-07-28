const rl = @import("raylib");
const std = @import("std");

const base_filepath = "./assets/";

fn checkExtension(filePath: [:0]const u8, extensions: []const [:0]const u8) bool {
    const fileExtension = filePath[filePath.len - 4 ..];
    for (extensions) |extension| {
        if (std.mem.eql(u8, fileExtension, extension)) {
            return true;
        }
    }
    return false;
}

fn fixFilePath(allocator: std.mem.Allocator, filePath: [:0]const u8) ![:0]const u8 {
    var fullFilePath: [:0]u8 = try allocator.allocSentinel(u8, base_filepath.len + filePath.len, 0);

    std.mem.copyForwards(u8, fullFilePath[0..], base_filepath);
    std.mem.copyForwards(u8, fullFilePath[base_filepath.len..], filePath);

    return fullFilePath;
}

pub fn image(imageFilePath: [:0]const u8) !rl.Image {
    const allocator: std.mem.Allocator = std.heap.c_allocator;
    const fullFilePath = try fixFilePath(allocator, imageFilePath);
    defer allocator.free(fullFilePath);
    if (checkExtension(imageFilePath, &.{ ".png", ".jpg" })) {
        return rl.loadImage(fullFilePath);
    } else {
        std.debug.print("Error trying to load image: .{s}", .{fullFilePath});
        return error.could_not_load_image;
    }
}

pub fn model(modelFilePath: [:0]const u8) !rl.Model {
    const allocator: std.mem.Allocator = std.heap.c_allocator;
    const fullFilePath = try fixFilePath(allocator, modelFilePath);
    defer allocator.free(fullFilePath);
    if (checkExtension(modelFilePath, &.{ ".glb", ".obj" })) {
        return rl.loadModel(fullFilePath);
    } else {
        std.debug.print("Error trying to load model: .{s}", .{fullFilePath});
        return error.could_not_load_model;
    }
}

pub fn texture(textureFilePath: [:0]const u8) !rl.Texture2D {
    const allocator: std.mem.Allocator = std.heap.c_allocator;
    const fullFilePath = try fixFilePath(allocator, textureFilePath);
    defer allocator.free(fullFilePath);
    if (checkExtension(textureFilePath, &.{ ".png", ".jpg" })) {
        return rl.loadTexture(fullFilePath);
    } else {
        std.debug.print("Error trying to load texture: .{s}", .{fullFilePath});
        return error.could_not_load_texture;
    }
}
