const Equipment = @import("equipment.zig");
const Potion = @import("potion.zig");
const resource = @import("resource.zig");

pub const Entity = union(enum) {
    equipment: Equipment,
    potion: Potion,
    resource: resource.Entity,   
};
