const std = @import("std");
const opcodes = @import("opcodes.zig");

pub const CpuError = error{
    OutOfMemory,
    InvalidOpCode,
    InvalidAddressing,
};

pub const AddressingMode = enum {
    Immediate,
    ZeroPage,
    ZeroPage_X,
    ZeroPage_Y,
    Absolute,
    Absolute_X,
    Absolute_Y,
    Indirect_X,
    Indirect_Y,
    NoneAddressing,
};

pub const CPU = struct {
    register_a: u8,
    register_x: u8,
    register_y: u8,
    status: u8,
    program_counter: u16,
    memory: [65536]u8,
    opcodes: opcodes.OpCodesMap,
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) !*CPU {
        const cpu: *CPU = try allocator.create(CPU);
        cpu.* = .{
            .register_a = 0,
            .register_x = 0,
            .register_y = 0,
            .status = 0,
            .program_counter = 0,
            .memory = [_]u8{0} ** 65536,
            .opcodes = try opcodes.createOpcodesMap(allocator),
            .allocator = allocator,
        };
        return cpu;
    }

    pub fn deinit(self: *CPU) void {
        self.opcodes.deinit();
        self.allocator.destroy(self);
    }

    fn mem_read(self: *const CPU, addr: u16) u8 {
        return self.memory[addr];
    }

    fn mem_write(self: *CPU, addr: u16, data: u8) void {
        self.memory[addr] = data;
    }

    fn mem_read_u16(self: *const CPU, pos: u16) u16 {
        const lo = @as(u16, self.mem_read(pos));
        const hi = @as(u16, self.mem_read(pos + 1));
        return (hi << 8) | lo;
    }

    fn mem_write_u16(self: *CPU, pos: u16, data: u16) void {
        const hi = @as(u8, @truncate(data >> 8));
        const lo = @as(u8, @truncate(data & 0xff));
        self.mem_write(pos, lo);
        self.mem_write(pos + 1, hi);
    }

    pub fn lda(self: *CPU, mode: AddressingMode) void {
        const addr = self.get_operand_address(mode);
        const value = self.mem_read(addr);
        self.register_a = value;
        self.updateZeroAndNegativeFlags(self.register_a);
    }

    fn tax(self: *CPU) void {
        self.register_x = self.register_a;
        self.updateZeroAndNegativeFlags(self.register_x);
    }

    fn inx(self: *CPU) void {
        self.register_x = self.register_x +% 1; // wrapping add
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

    pub fn reset(self: *CPU) void {
        self.register_a = 0;
        self.register_x = 0;
        self.register_y = 0;
        self.status = 0;

        self.program_counter = self.mem_read_u16(0xFFFC);
    }

    pub fn load(self: *CPU, program: []const u8) !void {
        if (program.len + 0x8000 > self.memory.len) return error.OutOfMemory;
        @memcpy(self.memory[0x8000..][0..program.len], program);
        self.mem_write_u16(0xFFFC, 0x8000);
    }

    pub fn load_and_run(self: *CPU, program: []const u8) !void {
        try self.load(program);
        self.reset();
        try self.run();
    }

    pub fn get_operand_address(self: *const CPU, mode: AddressingMode) u16 {
        return switch (mode) {
            .Immediate => self.program_counter,
            .ZeroPage => @as(u16, self.mem_read(self.program_counter)),
            .Absolute => self.mem_read_u16(self.program_counter),
            .ZeroPage_X => {
                const pos = self.mem_read(self.program_counter);
                return @as(u16, pos +% self.register_x);
            },
            .ZeroPage_Y => {
                const pos = self.mem_read(self.program_counter);
                return @as(u16, pos +% self.register_y);
            },
            .Absolute_X => {
                const base = self.mem_read_u16(self.program_counter);
                return base +% @as(u16, self.register_x);
            },
            .Absolute_Y => {
                const base = self.mem_read_u16(self.program_counter);
                return base +% @as(u16, self.register_y);
            },
            .Indirect_X => {
                const base = self.mem_read(self.program_counter);
                const ptr = base +% self.register_x;
                const lo = self.mem_read(@as(u16, ptr));
                const hi = self.mem_read(@as(u16, ptr +% 1));
                return (@as(u16, hi) << 8) | @as(u16, lo);
            },
            .Indirect_Y => {
                const base = self.mem_read(self.program_counter);
                const lo = self.mem_read(@as(u16, base));
                const hi = self.mem_read(@as(u16, base +% 1));
                const deref_base = (@as(u16, hi) << 8) | @as(u16, lo);
                return deref_base +% @as(u16, self.register_y);
            },
            .NoneAddressing => {
                std.debug.panic("mode {any} is not supported", .{mode});
            },
        };
    }

    pub fn run(self: *CPU) !void {
        while (true) {
            const code = self.mem_read(self.program_counter);
            self.program_counter += 1;
            const program_counter_state = self.program_counter;

            const opcode = self.opcodes.get(code) orelse return error.InvalidOpCode;

            switch (opcode.code) {
                0xa9, 0xa5, 0xb5, 0xad, 0xbd, 0xb9, 0xa1, 0xb1 => {
                    self.lda(opcode.mode);
                },
                0xAA => self.tax(),
                0xe8 => self.inx(),
                0x00 => return,
                else => @panic("Unknown opscode"),
            }

            if (program_counter_state == self.program_counter) {
                self.program_counter += @as(u16, opcode.len - 1);
            }
        }
    }
};

test "0xa9 lda immediate load data" {
    const allocator = std.heap.page_allocator;
    var cpu = try CPU.init(allocator);
    defer cpu.deinit();
    try cpu.load_and_run(&[_]u8{ 0xa9, 0x05, 0x00 });
    try std.testing.expectEqual(@as(u8, 5), cpu.register_a);
    try std.testing.expect((cpu.status & 0b0000_0010) == 0);
    try std.testing.expect((cpu.status & 0b1000_0000) == 0);
}

test "0xa9 lda zero flag" {
    const allocator = std.heap.page_allocator;
    var cpu = try CPU.init(allocator);
    defer cpu.deinit();
    try cpu.load_and_run(&[_]u8{ 0xa9, 0x00, 0x00 });
    try std.testing.expect((cpu.status & 0b0000_0010) == 0b10);
}

test "0xa9 lda negative flag" {
    const allocator = std.heap.page_allocator;
    var cpu = try CPU.init(allocator);
    defer cpu.deinit();
    try cpu.load_and_run(&[_]u8{ 0xa9, 0xff, 0x00 });
    try std.testing.expect((cpu.status & 0b1000_0000) == 0b1000_0000);
}

test "LDA from memory" {
    const allocator = std.heap.page_allocator;
    var cpu = try CPU.init(allocator);
    defer cpu.deinit();
    cpu.mem_write(0x10, 0x55);

    // In Zig we need to create a slice from an array
    const program = [_]u8{ 0xa5, 0x10, 0x00 };
    try cpu.load_and_run(&program);

    try std.testing.expectEqual(@as(u8, 0x55), cpu.register_a);
}

test "0xaa tax move a to x" {
    const allocator = std.heap.page_allocator;
    var cpu = try CPU.init(allocator);
    defer cpu.deinit();
    try cpu.load_and_run(&[_]u8{ 0xa9, 0x0A, 0xaa, 0x00 });
    try std.testing.expectEqual(@as(u8, 10), cpu.register_x);
}

test "5 ops working together" {
    const allocator = std.heap.page_allocator;
    var cpu = try CPU.init(allocator);
    defer cpu.deinit();
    try cpu.load_and_run(&[_]u8{ 0xa9, 0xc0, 0xaa, 0xe8, 0x00 });
    try std.testing.expectEqual(@as(u8, 0xc1), cpu.register_x);
}

test "inx overflow" {
    const allocator = std.heap.page_allocator;
    var cpu = try CPU.init(allocator);
    defer cpu.deinit();
    try cpu.load_and_run(&[_]u8{ 0xa9, 0xff, 0xaa, 0xe8, 0xe8, 0x00 });
    try std.testing.expectEqual(@as(u8, 1), cpu.register_x);
}
