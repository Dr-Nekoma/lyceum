const messages = @import("../../server/messages.zig");

pub const Entity = union(messages.World.Object) {
    empty: void,
    bush: u8,
    tree: u8,
    chest: u8,
    rock: u8,
};
