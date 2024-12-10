

pub const Kind = enum {
    head,
    top,
    bottom,
    feet,
    arms,
    finger,
};

name: [:0]const u8,
description: [:0]const u8,
kind: Kind,
