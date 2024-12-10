const messages = @import("../server/messages.zig");

pub const Entity = union(messages.Resource.Kind) {
    stone: u8,
    wood: u8,
}
