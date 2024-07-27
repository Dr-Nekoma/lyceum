const rl = @import("raylib");
const std = @import("std");

const base_filepath = "./assets/";

// TODO: Check if file is actually image before loading with raylib
// TODO: Make all these functions generic passing a lambda for the predicate as validation for each file type
pub fn image(imageFilePath: [:0]const u8) !rl.Image {
    const allocator: std.mem.Allocator = std.heap.c_allocator;

    var fullFilePath: [:0]u8 = try allocator.allocSentinel(u8, base_filepath.len + imageFilePath.len, 0);
    defer allocator.free(fullFilePath);

    std.mem.copyForwards(u8, fullFilePath[0..], base_filepath);
    std.mem.copyForwards(u8, fullFilePath[base_filepath.len..], imageFilePath);

    return rl.loadImage(fullFilePath);
}

pub fn model(modelFilePath: [:0]const u8) !rl.Model {
    const allocator: std.mem.Allocator = std.heap.c_allocator;

    var fullFilePath: [:0]u8 = try allocator.allocSentinel(u8, base_filepath.len + modelFilePath.len, 0);
    defer allocator.free(fullFilePath);

    std.mem.copyForwards(u8, fullFilePath[0..], base_filepath);
    std.mem.copyForwards(u8, fullFilePath[base_filepath.len..], modelFilePath);

    return rl.loadModel(fullFilePath);
}

pub fn texture(textureFilePath: [:0]const u8) !rl.Texture2D {
    const allocator: std.mem.Allocator = std.heap.c_allocator;

    var fullFilePath: [:0]u8 = try allocator.allocSentinel(u8, base_filepath.len + textureFilePath.len, 0);
    defer allocator.free(fullFilePath);

    std.mem.copyForwards(u8, fullFilePath[0..], base_filepath);
    std.mem.copyForwards(u8, fullFilePath[base_filepath.len..], textureFilePath);

    return rl.loadTexture(fullFilePath);
}
