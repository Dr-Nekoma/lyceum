const rl = @import("raylib");
const GameState = @import("../game/state.zig");
const GameCharacter = @import("../game/character.zig");

pub const character = struct {
    pub fn update(entity: *GameCharacter) void {
        const anims = entity.animation.frames;
        if (anims.len >= 0) {
            switch (entity.stats.state_type) {
                .walking => {
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
