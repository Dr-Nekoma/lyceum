const rl = @import("raylib");
const std = @import("std");
const zerl = @import("zerl");
const GameState = @import("../game/state.zig");
const GameCharacter = @import("../game/character.zig");

fn createAnonymousStruct(comptime T: type, comptime keys: []const [:0]const u8) type {
    const struct_info = @typeInfo(T).Struct;
    comptime var structKeys: [keys.len]std.meta.Tuple(&.{[:0]const u8}) = undefined;
    comptime for (0.., keys) |index, key| {
        structKeys[index] = .{key};
    };
    const mapKeys = std.StaticStringMap(void).initComptime(structKeys);
    return comptime blk: {
        var fields: [keys.len]std.builtin.Type.StructField = undefined;
        var fieldsCounter: usize = 0;
        for (struct_info.fields) |field| {
            if (mapKeys.has(field.name)) {
                fields[fieldsCounter] = field;
                fieldsCounter += 1;
            }
        }
        break :blk @Type(.{
            .Struct = .{
                .layout = .auto,
                .fields = &fields,
                .decls = &[_]std.builtin.Type.Declaration{},
                .is_tuple = false,
            },
        });
    };
}

pub fn selectKeysFromStruct(data: anytype, comptime keys: []const [:0]const u8) createAnonymousStruct(@TypeOf(data), keys) {
    const Temp: type = createAnonymousStruct(@TypeOf(data), keys);
    var anonymousStruct: Temp = undefined;
    inline for (@typeInfo(Temp).Struct.fields) |field| {
        const current_field = &@field(anonymousStruct, field.name);
        current_field.* = @field(data, field.name);
    }
    return anonymousStruct;
}

// Standard Response from Erlang Server

fn TupleResponse(comptime T: type) type {
    return union(enum) {
        ok: T,
        @"error": [:0]const u8,
    };
}

pub const ErlangResponse = TupleResponse(void);

// User's Login and Registration

pub const User = struct {
    pub const Login = struct {
        pub const Request = struct {
            username: []const u8,
            password: []const u8,
        };

        const Info = std.meta.Tuple(&.{ zerl.ei.erlang_pid, [:0]const u8 });
        pub const Response = TupleResponse(Info);
    };

    pub const Registry = struct {
        pub const Request = struct {
            username: [:0]const u8,
            email: [:0]const u8,
            password: [:0]const u8,
        };

        // TODO: Implement user registration via the client
        pub const Response = TupleResponse(void);
    };
};

pub const World = struct {
    // User's Characters

    pub const Tile = enum {
        empty,
        water,
        grass,
        sand,
        dirt,
    };

    pub const Object = enum {
        empty,
        bush,
        tree,
        chest,
        rock,
    };

    pub const Position = struct { f32, f32 };

    pub const Resource = struct {
        kind: Object = .empty,
        quantity: u32 = 50,
        capacity: u32 = 50,
        base_extraction_amount: u32 = 1,
        base_extraction_time: u32 = 1,
        item_pk: [:0]const u8 = "",
    };

    pub const ResourceLocation = struct { Position, Resource };

    pub const Map = struct {
        width: u32 = 10,
        height: u32 = 10,
        tiles: []const Tile = &.{},
        objects: []const Object = &.{},
        resources: []const ResourceLocation = &.{},
    };
};

pub const Character = struct {
    pub const Harvest = struct {
        pub const Request = struct {
            name: [:0]const u8,
            username: []const u8,
            email: []const u8,
            map_name: [:0]const u8,
            kind: World.Object,
            x_position: f32,
            y_position: f32,
        };

        pub const Info = struct {
            delta_inventory: struct {
                item_name: [:0]const u8 = "",
                quantity: u32 = 0,
            },
            delta_resource: u32 = 0,
        };

        pub const Response = TupleResponse(Harvest.Info);
    };

    pub const Join = struct {
        const Info = struct {
            character: Character.Info,
            map: World.Map,
        };

        pub const Response = TupleResponse(Join.Info);

        pub const Request = struct {
            username: []const u8,
            name: []const u8,
            email: []const u8,
            map_name: []const u8,
        };
    };

    pub const Update = struct {
        level: u8,
        health: u16,
        mana: u16,
        name: [:0]const u8,
        x_position: f32,
        y_position: f32,
        x_velocity: f32,
        y_velocity: f32,
        map_name: [:0]const u8,
        email: []const u8,
        username: []const u8,
        face_direction: i16,
        state_type: GameCharacter.State,
    };

    pub const Info = struct {
        level: u8 = 0,
        health: u16 = 0,
        health_max: u16 = 100,
        mana: u16 = 0,
        mana_max: u16 = 100,
        name: [:0]const u8 = "",
        constitution: u8 = 0,
        wisdom: u8 = 0,
        endurance: u8 = 0,
        strength: u8 = 0,
        intelligence: u8 = 0,
        faith: u8 = 0,
        x_position: f32 = 0,
        y_position: f32 = 0,
        x_velocity: f32 = 0,
        y_velocity: f32 = 0,
        face_direction: i16 = 270,
        map_name: [:0]const u8 = "",
        state_type: GameCharacter.State = .idle,
    };

    pub const Many = struct {
        pub const Request = struct {
            username: []const u8,
            email: []const u8,
        };

        pub const Response = TupleResponse([]const Info);
    };
};

// Central place to send game's data

pub const Payload = union(enum) {
    debug: [:0]const u8,
    exit_map: void,
    joining_map: Character.Join.Request,
    list_characters: Character.Many.Request,
    login: User.Login.Request,
    logout: void,
    register: User.Registry.Request,
    // create_character:
    update_character: Character.Update,
    harvest_resource: Character.Harvest.Request,
};
