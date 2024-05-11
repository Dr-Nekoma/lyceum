const erl = @import("erlang.zig");
const std = @import("std");
const rl = @import("raylib");

// pub fn main() !void {
//     const connection_status = erl.ei.ei_init();
//     if (connection_status != 0) return error.ei_init_failed;
//     var node: erl.LNode = try erl.prepare_connection();
//     try erl.establish_connection(&node);
//     try erl.send_payload(&node, .{
//         .user_registry = .{
//             .username = "Nathan",
//             .email = "zcxv@bnm.drn",
//             .password = "3756849034567890",
//         },
//     });
//     const msg: []const u8 = try erl.receive_message(&node);
//     std.debug.print("{s}", .{msg});
// }

const fontSize = 20;
pub const ColorPalette = struct {
    pub const primary = rl.Color.init(13, 25, 23, 255);
    pub const secondary = rl.Color.init(222, 222, 222, 255);
    pub const background = rl.Color.init(68, 105, 129, 255);
};

pub fn drawButton(message: [:0]const u8, position: rl.Vector2, size: rl.Vector2, color: rl.Color) !void {
    rl.drawRectangleV(position, size, color);
    const messageSize: f32 = @floatFromInt(rl.measureText(message, fontSize));
    const messageX = position.x + size.x / 2 - messageSize / 2;
    const floatFont: f32 = @floatFromInt(fontSize);
    const messageY = position.y + size.y / 2 - floatFont / 2;
    rl.drawText(message, @intFromFloat(messageX), @intFromFloat(messageY), fontSize, ColorPalette.secondary);
}

pub const GameState = struct {
    sceneState: enum {
        user_registry,
        nothing,        
    },
    width: f32,
    height: f32,
    buttons: buttonMapType,
    const buttonMapType = std.hash_map.AutoHashMap(rl.Vector4, AvailableButtons);
    pub const AvailableButtons = enum {
        user_registry
    };

    pub fn init(allocator: std.mem.Allocator, width: f32, height: f32) !GameState {
        if (width < 0) {
            return error.negative_width;
        }
        if (height < 0) {
            return error.negative_height;
        }
        return .{
            .sceneState = .nothing,
            .width = width,
            .height = height,
            .buttons = buttonMapType.init(allocator),
        };
    }

    pub fn deinit(gameState: * @This()) void {
        gameState.buttons.deinit();
    } 
        
    pub fn drawMainMenu(gameState: *const @This()) anyerror!void {
        const userCreationButtonHeight: f32 = gameState.height / 8;
        const userCreationButtonWidth: f32 = gameState.width / 4;
        try drawButton("Create User",
                       .{.x = (gameState.width / 2) - (userCreationButtonWidth / 2),
                         .y = (gameState.height / 2) - (userCreationButtonHeight / 2)},
                       .{.x = userCreationButtonWidth,
                         .y = userCreationButtonHeight},
                       ColorPalette.primary);
        // TODO: Judge whether we should do Raylib-way for effect-detection, i.e., you draw and check together, or the pure way, i.e., you do logic and effects separately.
    }

    pub fn drawCreateUser(gameState: *const @This()) anyerror!void {
        rl.drawText("Congrats! You created your first window!", @intFromFloat(gameState.width / 4), @intFromFloat(gameState.height / 2), fontSize, rl.Color.black);
    }
};

pub fn main() anyerror!void {
    var gameState = try GameState.init(std.heap.c_allocator, 800, 450);
    defer gameState.deinit();

    rl.setConfigFlags(.flag_window_resizable);
    rl.initWindow(@intFromFloat(gameState.width), @intFromFloat(gameState.height), "Lyceum");   
    defer rl.closeWindow();
    rl.setTargetFPS(60);
    
    while (!rl.windowShouldClose()) {
        rl.beginDrawing();
        defer rl.endDrawing();

        rl.clearBackground(ColorPalette.background);
        gameState.width = @floatFromInt(rl.getScreenWidth());
        gameState.height = @floatFromInt(rl.getScreenHeight());

        try GameState.drawMainMenu(&gameState);       
    }   
}
