const std = @import("std");
const CPU = @import("cpu.zig").CPU;
const opcodes = @import("opcodes.zig");

pub fn main() !void {
    std.debug.print("Hello, world!\n", .{});
}

test "run all tests" {
    std.testing.refAllDecls(@This());
}
