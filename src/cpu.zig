const std = @import("std");

pub const CPU = struct {
    register_a: u8,
    register_x: u8,
    status: u8,
    program_counter: u16,

    pub fn init() CPU {
        return CPU{
            .register_a = 0,
            .register_x = 0,
            .status = 0,
            .program_counter = 0,
        };
    }

    fn lda(self: *CPU, value: u8) void {
        self.register_a = value;
        self.updateZeroAndNegativeFlags(self.register_a);
    }

    fn tax(self: *CPU) void {
        self.register_x = self.register_a;
        self.updateZeroAndNegativeFlags(self.register_x);
    }

    fn updateZeroAndNegativeFlags(self: *CPU, result: u8) void {
        if (result == 0) {
            self.status |= 0b0000_0010;
        } else {
            self.status &= 0b1111_1101;
        }

        if ((result & 0b1000_0000) != 0) {
            self.status |= 0b1000_0000;
        } else {
            self.status &= 0b0111_1111;
        }
    }

    fn inx(self: *CPU) void {
        self.register_x = self.register_x +% 1; // wrapping add
        self.updateZeroAndNegativeFlags(self.register_x);
    }

    pub fn interpret(self: *CPU, program: []const u8) void {
        self.program_counter = 0;

        while (true) {
            const opscode = program[@intCast(self.program_counter)];
            self.program_counter += 1;

            switch (opscode) {
                0xA9 => {
                    const param = program[@intCast(self.program_counter)];
                    self.program_counter += 1;
                    self.lda(param);
                },
                0xAA => self.tax(),
                0xe8 => self.inx(),
                0x00 => return,
                else => @panic("Unknown opscode"),
            }
        }
    }
};

test "0xa9 lda immediate load data" {
    var cpu = CPU.init();
    cpu.interpret(&[_]u8{ 0xa9, 0x05, 0x00 });
    try std.testing.expectEqual(@as(u8, 5), cpu.register_a);
    try std.testing.expect((cpu.status & 0b0000_0010) == 0);
    try std.testing.expect((cpu.status & 0b1000_0000) == 0);
}

test "0xa9 lda zero flag" {
    var cpu = CPU.init();
    cpu.interpret(&[_]u8{ 0xa9, 0x00, 0x00 });
    try std.testing.expect((cpu.status & 0b0000_0010) == 0b10);
}

test "0xa9 lda negative flag" {
    var cpu = CPU.init();
    cpu.interpret(&[_]u8{ 0xa9, 0xff, 0x00 });
    try std.testing.expect((cpu.status & 0b1000_0000) == 0b1000_0000);
}

test "0xaa tax move a to x" {
    var cpu = CPU.init();
    cpu.register_a = 10;
    cpu.interpret(&[_]u8{ 0xaa, 0x00 });
    try std.testing.expectEqual(@as(u8, 10), cpu.register_x);
}

test "5 ops working together" {
    var cpu = CPU.init();
    cpu.interpret(&[_]u8{ 0xa9, 0xc0, 0xaa, 0xe8, 0x00 });
    try std.testing.expectEqual(@as(u8, 0xc1), cpu.register_x);
}

test "inx overflow" {
    var cpu = CPU.init();
    cpu.register_x = 0xff;
    cpu.interpret(&[_]u8{ 0xe8, 0xe8, 0x00 });
    try std.testing.expectEqual(@as(u8, 1), cpu.register_x);
}
