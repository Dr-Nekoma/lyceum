
pub const Kind = enum {
    health,
    mana,
    poison,
    effect,
};

name: [:0]const u8,
description: [:0]const u8,
kind: Kind,
