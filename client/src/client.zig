const erl = @import("erlang.zig");
const std = @import("std");
const rl = @import("raylib");

// pub fn main() !void {
//     const connection_status = erl.ei.ei_init();
//     if (connection_status != 0) return error.ei_init_failed;
//     var node: erl.LNode = try erl.prepare_connection();
//     try erl.establish_connection(&node);
//     try erl.send_message(&node, "Hello from Lyceum!");
//     var msg: []const u8 = try erl.receive_message(&node);
//     std.debug.print("{s}", .{msg});
// }


pub fn main() anyerror!void {
    // Initialization
    //--------------------------------------------------------------------------------------
    const screenWidth = 800;
    const screenHeight = 450;

    rl.initWindow(screenWidth, screenHeight, "raylib-zig [core] example - basic window");
    defer rl.closeWindow(); // Close window and OpenGL context

    rl.setTargetFPS(60); // Set our game to run at 60 frames-per-second
    //--------------------------------------------------------------------------------------

    // Main game loop
    while (!rl.windowShouldClose()) { // Detect window close button or ESC key
        // Update
        //----------------------------------------------------------------------------------
        // TODO: Update your variables here
        //----------------------------------------------------------------------------------

        // Draw
        //----------------------------------------------------------------------------------
        rl.beginDrawing();
        defer rl.endDrawing();

        rl.clearBackground(rl.Color.white);

        rl.drawText("Congrats! You created your first window!", 190, 200, 20, rl.Color.light_gray);
        //----------------------------------------------------------------------------------
    }
}
