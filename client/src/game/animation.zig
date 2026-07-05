const rl = @import("raylib");
const GameState = @import("../game/state.zig");
const GameCharacter = @import("../game/character.zig");

pub const character = struct {
    pub fn update(entity: *GameCharacter) void {
        if (entity.animation.frames.len >= 0) {
            switch (entity.stats.state_type) {
                .walking => {
                    const animFrameCounter = &entity.animation.frameCounter;
                    if (entity.model) |model| {
                        rl.updateModelAnimation(
                            model,
                            entity.animation.frames[1],
                            @floatFromInt(animFrameCounter.*),
                        );
                    }
                    animFrameCounter.* += 1;

                    const frameCount: i32 = entity.animation.frameCounter;
                    if (animFrameCounter.* >= frameCount) animFrameCounter.* = 0;
                },
                .idle, .collecting_resource => {
                    if (entity.model) |model| rl.updateModelAnimation(
                        model,
                        entity.animation.frames[0],
                        0,
                    );
                },
            }
        }
    }
};
