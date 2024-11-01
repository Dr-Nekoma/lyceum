const rl = @import("raylib");

pub const angleCameraVector: rl.Vector3 = .{
    .x = 25,
    .y = 30,
    .z = 20,
};

pub const map = struct {
    pub const max_width = 1000;
    pub const border_thickness = 140;
    pub const max_height = 1000;
};

pub const tileSize = 100;
pub const objectSize: rl.Vector3 = .{
    .x = 100,
    .y = tileSize,
    .z = 100,
};

pub const assets = struct {
    pub const tile = struct {
        pub const scale: rl.Vector3 = .{
            .x = 0.25,
            .y = 0.25,
            .z = 0.25,
        };
        pub const angle = -90;
        pub const axis: rl.Vector3 = .{
            .x = 1,
            .y = 0,
            .z = 0,
        };
    };
};

pub const defaultCameraDistance = 72;
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
