const complex = @cImport(
    @cInclude("complex.c"),
);

const erl_comm = @cImport(
    @cInclude("erl_comm.c"),
);

pub fn main() !void {
    var f: i32 = undefined;
    var arg: i32 = undefined;
    var res: i32 = undefined;
    var buf: [100]u8 = undefined;

    while (erl_comm.read_cmd(&buf) > 0) {
        f = buf[0];
        arg = buf[1];

        res = switch (f) {
            1 => complex.foo(arg),
            2 => complex.bar(arg),
            else => res,
        };

        buf[0] = @truncate(@as(u32, @bitCast(res)));
        _ = erl_comm.write_cmd(&buf, 1);
    }
}
