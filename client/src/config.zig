const rl = @import("raylib");

pub const angleCameraVector: rl.Vector3 = .{
    .x = -25,
    .y = 30,
    .z = 25,
};

pub const map = struct {
    pub const border_thickness = 140;
    pub const mini_map_size = 50;
};

pub const assets = struct {
    pub const tile = struct {
        pub const level = 9;
        pub const size = 12;
        pub const scale: rl.Vector3 = .{
            .x = 0.04,
            .y = 0.04,
            .z = 0.04,
        };
        pub const angle = -90;
        pub const axis: rl.Vector3 = .{
            .x = 1,
            .y = 0,
            .z = 0,
        };
    };
    pub const object = struct {
        pub const defaultLevel = 10;
        pub const defaultAngle = -90;
        pub const defaultAxis: rl.Vector3 = .{
            .x = 1,
            .y = 0,
            .z = 0,
        };
        pub const chest = struct {
            pub const scale: rl.Vector3 = .{
                .x = 0.15,
                .y = 0.15,
                .z = 0.15,
            };
            pub const angle = defaultAngle;
            pub const axis = defaultAxis;
        };
        pub const bush = struct {
            pub const scale: rl.Vector3 = .{
                .x = 2.05,
                .y = 2.05,
                .z = 2.05,
            };
            pub const angle = defaultAngle;
            pub const axis = defaultAxis;
        };
        pub const tree = struct {
            pub const scale: rl.Vector3 = .{
                .x = 2.0,
                .y = 2.0,
                .z = 2.0,
            };
            pub const angle = defaultAngle;
            pub const axis: rl.Vector3 = .{
                .x = 0,
                .y = 1,
                .z = 0,
            };
        };
    };
    pub const paths = struct {
        pub const menu = struct {
            pub const connection = struct {
                pub const connected = "menu/connection/connected.png";
                pub const notConnected = "menu/connection/not-connected.png";
            };
            pub const character = struct {
                pub const placeholder = "menu/character/placeholder.png";
            };
            pub const music = struct {
                pub const background = "menu/music/03-Linear-B.ogg";
            };
            pub const logo = "menu/logo.png";
        };
        pub const game = struct {
            pub const character = struct {
                pub const walker = "game/character/walker.m3d";
                pub const knight = "game/character/knight.glb";
            };
            pub const world = struct {
                pub const tiles = struct {
                    pub const dirt = struct {
                        pub const model = "game/world/tiles/dirt/dirt.obj";
                        pub const img = "game/world/tiles/dirt/dirt.png";
                    };
                    pub const grass = struct {
                        pub const model = "game/world/tiles/grass/grass.obj";
                        pub const img = "game/world/tiles/grass/grass.png";
                    };
                    pub const sand = struct {
                        pub const model = "game/world/tiles/sand/sand.obj";
                        pub const img = "game/world/tiles/sand/sand.png";
                    };
                    pub const water = struct {
                        pub const model = "game/world/tiles/water/water.obj";
                        pub const img = "game/world/tiles/water/water.png";
                    };
                };
                pub const objects = struct {
                    pub const chest = struct {
                        pub const model = "game/world/objects/chest/chest.obj";
                    };
                    pub const tree = struct {
                        pub const model = "game/world/objects/tree/tree.glb";
                    };
                    pub const bush = struct {
                        pub const model = "game/world/objects/bush/bush.obj";
                    };
                };
            };
        };
    };
};

pub const fov = 2;
pub const defaultCameraDistance = 35;
pub const buttonFontSize = 20;
pub const textFontSize = 20;
pub const hubFontSize = 6;
pub const menuButtonsPadding = 6;
pub const nameSize = 18;
pub const messageSize = 50;
pub const maximumCharacters = 6;
pub const Screen = struct {
    pub const initialWidth = 800;
    pub const initialHeight = 450;
};
pub const ColorPalette = struct {
    pub const primary = rl.Color.init(13, 25, 23, 255);
    pub const secondary = rl.Color.init(222, 222, 222, 255);
    pub const background = rl.Color.init(68, 105, 129, 255);
    pub const disabled = rl.Color.init(86, 88, 92, 255);
    pub const connection_status = rl.Color.init(0, 0, 0, 127);
};
