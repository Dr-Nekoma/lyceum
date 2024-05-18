const rl = @import("raylib");

pub const buttonFontSize = 20;
pub const textFontSize = 20;
pub const menuButtonsPadding = 6;
pub const ColorPalette = struct {
    pub const primary = rl.Color.init(13, 25, 23, 255);
    pub const secondary = rl.Color.init(222, 222, 222, 255);
    pub const background = rl.Color.init(68, 105, 129, 255);
};
