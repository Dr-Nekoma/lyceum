const rl = @import("raylib");
const std = @import("std");

const base_filepath = "./assets/";

fn checkExtension(filePath: [:0]const u8, extensions: []const [:0]const u8) bool {
    const fileExtension = std.fs.path.extension(filePath);
    for (extensions) |extension| {
        if (std.mem.eql(u8, fileExtension, extension)) {
            return true;
        }
    }
    return false;
}

pub fn image(imageFilePath: [:0]const u8) !rl.Image {
    const allocator: std.mem.Allocator = std.heap.c_allocator;
    const fullFilePath = try std.fs.path.joinZ(allocator, &.{ base_filepath, imageFilePath });
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
    const fullFilePath = try std.fs.path.joinZ(allocator, &.{ base_filepath, modelFilePath });
    defer allocator.free(fullFilePath);
    if (checkExtension(modelFilePath, &.{ ".glb", ".obj", ".m3d" })) {
        return rl.loadModel(fullFilePath);
    } else {
        std.debug.print("Error trying to load model: .{s}", .{fullFilePath});
        return error.could_not_load_model;
    }
}

pub fn texture(textureFilePath: [:0]const u8) !rl.Texture2D {
    const allocator: std.mem.Allocator = std.heap.c_allocator;
    const fullFilePath = try std.fs.path.joinZ(allocator, &.{ base_filepath, textureFilePath });
    defer allocator.free(fullFilePath);
    if (checkExtension(textureFilePath, &.{ ".png", ".jpg" })) {
        return rl.loadTexture(fullFilePath);
    } else {
        std.debug.print("Error trying to load texture: .{s}", .{fullFilePath});
        return error.could_not_load_texture;
    }
}

pub fn animations(animationFilePath: [:0]const u8) ![]rl.ModelAnimation {
    const allocator: std.mem.Allocator = std.heap.c_allocator;
    const fullFilePath = try std.fs.path.joinZ(allocator, &.{ base_filepath, animationFilePath });
    defer allocator.free(fullFilePath);
    if (checkExtension(animationFilePath, &.{".m3d"})) {
        return try rl.loadModelAnimations(fullFilePath);
    } else {
        std.debug.print("Error trying to load animation model: .{s}", .{fullFilePath});
        return error.could_not_load_animation;
    }
}
