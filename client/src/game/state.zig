const assets = @import("../assets.zig");
const chat = @import("../components/hud/Chat.zig");
const config = @import("../config.zig");
const errorC = @import("../components/error.zig");
const hud = @import("../components/hud/main.zig");
const mainMenu = @import("../menu/main.zig");
const map = @import("../components/hud/map.zig");
const messages = @import("../server/messages.zig");
const physics = @import("physics.zig");
const rl = @import("raylib");
const std = @import("std");
const zerl = @import("zerl");

// TODO: Make this a tagged union in which we have different data available
// per scene, so we can have more guarantees of what is happening with the data
// This should allow us to not have nullables everywhere.
pub const Scene = enum {
    user_registry,
    user_login,
    nothing,
    spawn,
    character_selection,
    connect,
};

pub const Character_Table = std.StringHashMap(World.Character);
pub const Tile_Table = std.EnumMap(messages.Tile, struct { ?rl.Model, ?rl.Image });
pub const Object_Table = std.EnumMap(messages.Object, assets.Object);

pub const World = struct {
    pub const Chat = struct {
        pub const bufferSize = 50;
        content: [bufferSize:0]u8 = .{0} ** bufferSize,
        messages: std.ArrayList(chat.Message) = std.ArrayList(chat.Message).init(std.heap.c_allocator),
        position: usize = 0,
        mode: chat.Mode = .idle,
    };
    pub const Map = struct {
        instance: messages.Map = .{},
        tiles: Tile_Table,
        objects: Object_Table,
    };
    pub const Character = struct {
        pub const Animation = struct {
            pub const State = enum {
                walking,
                idle,
            };
            frameCounter: i32 = 0,
            frames: []rl.ModelAnimation = &.{},
        };
        animation: Animation = .{},
        stats: messages.Character_Info = .{},
        model: ?rl.Model = null,
        // TODO: Remove this position and use spatial info from stats
        position: rl.Vector3 = .{
            .x = 0.0,
            .y = physics.character.floorLevel,
            .z = 0.0,
        },
        preview: ?rl.Texture2D = null,
        velocity: rl.Vector3 = .{
            .x = 0,
            .y = 0,
            .z = 0,
        },
        // TODO: These things should come from the server
        inventory: struct {
            items: []const [:0]const u8 = &.{},
            spells: []const [:0]const u8 = &.{},
            hud: struct {
                spells: []const [:0]const u8 = &.{},
                consumables: []const [:0]const u8 = &.{},
                minimap: struct {
                    map: ?rl.Image = null,
                    texture: ?rl.Texture = null,
                } = .{},
                chat: Chat = .{},
            } = .{},
        } = .{},
    };
    character: Character = .{},
    other_players: Character_Table,
    camera: rl.Camera = undefined,
    cameraDistance: f32 = config.defaultCameraDistance,
    map: Map,
};

pub const Connection = struct {
    pub const process_name = "lyceum_server";
    handler: ?zerl.ei.erlang_pid = null,
    node: *zerl.Node,
    is_connected: bool = false,
};

width: f32,
height: f32,
menu: mainMenu.Menu = undefined,
allocator: std.mem.Allocator = std.heap.c_allocator,
scene: Scene = .nothing,
connection: Connection,
world: World = undefined,
errorElem: *errorC,
music: struct {
    playing: bool = true,
    volume: f32 = 0.5,
} = .{},

pub fn send(state: *@This(), data: anytype) !void {
    try if (state.connection.handler) |*pid|
        state.connection.node.send(pid, data)
    else
        state.connection.node.send(Connection.process_name, data);
}

pub fn send_with_self(state: *@This(), message: messages.Payload) !void {
    try state.send(.{ try state.connection.node.self(), message });
}

pub fn canDisplayPlayer(mainPlayer: *const World.Character, player: *const World.Character) bool {
    const main_x_tile: i32 = @intFromFloat(mainPlayer.position.x / config.assets.tile.size);
    const main_y_tile: i32 = @intFromFloat(mainPlayer.position.z / config.assets.tile.size);
    const intXTile: i32 = @intFromFloat(player.position.x / config.assets.tile.size);
    const intYTile: i32 = @intFromFloat(player.position.z / config.assets.tile.size);
    const delta_x_tile = @abs(intXTile - main_x_tile);
    const delta_y_tile = @abs(intYTile - main_y_tile);

    return (delta_x_tile <= config.fov and delta_y_tile <= config.fov);
}

pub fn init(
    allocator: std.mem.Allocator,
    width: f32,
    height: f32,
    node: *zerl.Node,
    errorElem: *errorC,
) !@This() {
    const camera: rl.Camera = .{
        .position = .{ .x = 50.0, .y = 50.0, .z = 50.0 },
        .target = .{ .x = 0.0, .y = 10.0, .z = 0.0 },
        .up = .{ .x = 0.0, .y = 1.0, .z = 0.0 },
        .fovy = 45.0,
        .projection = .camera_perspective,
    };
    if (width < 0) return error.negative_width;
    if (height < 0) return error.negative_height;
    const name = try allocator.allocSentinel(u8, config.nameSize, 0);
    @memset(name, 0);
    return .{
        .width = width,
        .height = height,
        .allocator = allocator,
        .connection = .{ .node = node },
        .world = .{
            .camera = camera,
            .other_players = Character_Table.init(allocator),
            .character = .{
                .inventory = .{
                    .hud = .{
                        // TODO: use actual items
                        .spells = &.{ "item1", "item2", "item3", "item4", "item5" },
                        // TODO: use actual consumables
                        .consumables = &.{ "item6", "item7" },
                        .minimap = .{
                            .texture = map.init_map_texture(),
                        },
                    },
                },
            },
            .map = .{
                .tiles = try assets.tilesTable(),
                .objects = try assets.objectsTable(),
            },
        },
        .menu = .{
            .assets = try mainMenu.loadAssets(),
            .character = .{
                .create = .{
                    .name = name,
                },
            },
        },
        .errorElem = errorElem,
    };
}
