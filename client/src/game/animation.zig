const rl = @import("raylib");
const rm = rl.math;
const std = @import("std");
const GameState = @import("../game/state.zig");

pub const character = struct {
    pub fn update(entity: *GameState.World.Character) void {
        const anims = entity.animation.frames;
        if (anims.len >= 0) {
            switch (entity.stats.state_type) {
                .walking => {
                    if (std.mem.eql(u8, entity.stats.name, "Gaiseric")) std.debug.print("Character: {}\n", .{entity});
                    const animFrameCounter = &entity.animation.frameCounter;
                    if (entity.model) |model| rl.updateModelAnimation(model, anims[1], animFrameCounter.*);
                    animFrameCounter.* += 1;

                    const frameCount: i32 = @intCast(anims[1].frameCount);
                    if (animFrameCounter.* >= frameCount) animFrameCounter.* = 0;
                },
                .idle => {
                    if (entity.model) |model| rl.updateModelAnimation(model, anims[0], 0);
                },
            }
        }
    }
};
