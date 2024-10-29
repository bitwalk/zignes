const std = @import("std");
const opcodes = @import("opcodes.zig");

const MEMORY_SIZE = 0x10000;
const RESET_VECTOR = 0xFFFC;
const ROM_START = 0x8000;

const STACK_POINTER = 0x0100;
const STACK_RESET_VALUE = 0xFD;

pub const CpuFlags = union(enum) {
    bits: u8,

    pub const CARRY = 0b00000001;
    pub const ZERO = 0b00000010;
    pub const INTERRUPT_DISABLE = 0b00000100;
    pub const DECIMAL_MODE = 0b00001000;
    pub const BREAK = 0b00010000;
    pub const BREAK2 = 0b00100000;
    pub const OVERFLOW = 0b01000000;
    pub const NEGATIVE = 0b10000000;

    pub fn fromBitsTruncate(bits: u8) u8 {
        return bits;
    }

    pub fn contains(flags: u8, flag: u8) bool {
        return (flags & flag) != 0;
    }

    pub fn insert(flags: *u8, flag: u8) void {
        flags.* |= flag;
    }

    pub fn remove(flags: *u8, flag: u8) void {
        flags.* &= ~flag;
    }
};

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
    stack_pointer: u8,
    memory: [MEMORY_SIZE]u8,
    opcodes: opcodes.OpCodesMap,
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) !*CPU {
        const cpu: *CPU = try allocator.create(CPU);
        cpu.* = .{
            .register_a = 0,
            .register_x = 0,
            .register_y = 0,
            .status = CpuFlags.fromBitsTruncate(0b100100),
            .program_counter = 0,
            .stack_pointer = STACK_RESET_VALUE,
            .memory = [_]u8{0} ** MEMORY_SIZE,
            .opcodes = try opcodes.createOpcodesMap(allocator),
            .allocator = allocator,
        };
        return cpu;
    }

    pub fn deinit(self: *CPU) void {
        self.opcodes.deinit();
        self.allocator.destroy(self);
    }

    fn memRead(self: *const CPU, addr: u16) u8 {
        return self.memory[addr];
    }

    fn memWrite(self: *CPU, addr: u16, data: u8) void {
        self.memory[addr] = data;
    }

    fn memReadU16(self: *const CPU, pos: u16) u16 {
        const lo = @as(u16, self.memRead(pos));
        const hi = @as(u16, self.memRead(pos + 1));
        return (hi << 8) | lo;
    }

    fn memWriteU16(self: *CPU, pos: u16, data: u16) void {
        const hi = @as(u8, @truncate(data >> 8));
        const lo = @as(u8, @truncate(data & 0xff));
        self.memWrite(pos, lo);
        self.memWrite(pos + 1, hi);
    }

    fn lda(self: *CPU, mode: AddressingMode) void {
        const addr = self.getOperandAddress(mode);
        const value = self.memRead(addr);
        self.register_a = value;
        self.updateZeroAndNegativeFlags(self.register_a);
    }

    fn sta(self: *CPU, mode: AddressingMode) void {
        const addr = self.getOperandAddress(mode);
        self.memWrite(addr, self.register_a);
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
            CpuFlags.insert(&self.status, CpuFlags.ZERO);
        } else {
            CpuFlags.remove(&self.status, CpuFlags.ZERO);
        }

        if ((result & 0b1000_0000) != 0) {
            CpuFlags.insert(&self.status, CpuFlags.NEGATIVE);
        } else {
            CpuFlags.remove(&self.status, CpuFlags.NEGATIVE);
        }
    }

    fn rol(self: *CPU, mode: AddressingMode) !u8 {
        const addr = self.getOperandAddress(mode);
        var data = self.memRead(addr);
        const oldCarry = CpuFlags.contains(self.status, CpuFlags.CARRY);

        if (data >> 7 == 1) {
            self.setCarryFlag();
        } else {
            self.clearCarryFlag();
        }

        data = data << 1;
        if (oldCarry) {
            data |= 1;
        }

        self.memWrite(addr, data);
        self.updateZeroAndNegativeFlags(data);
        return data;
    }

    fn setCarryFlag(self: *CPU) void {
        CpuFlags.insert(&self.status, CpuFlags.CARRY);
    }

    fn clearCarryFlag(self: *CPU) void {
        CpuFlags.remove(&self.status, CpuFlags.CARRY);
    }

    pub fn reset(self: *CPU) void {
        self.register_a = 0;
        self.register_x = 0;
        self.register_y = 0;
        self.stack_pointer = STACK_RESET_VALUE;
        self.status = CpuFlags.fromBitsTruncate(0b100100);

        self.program_counter = self.memReadU16(RESET_VECTOR);
    }

    pub fn load(self: *CPU, program: []const u8) !void {
        if (program.len + ROM_START > self.memory.len) return error.OutOfMemory;
        @memcpy(self.memory[ROM_START..][0..program.len], program);
        self.memWriteU16(RESET_VECTOR, ROM_START);
    }

    pub fn loadAndRun(self: *CPU, program: []const u8) !void {
        try self.load(program);
        self.reset();
        try self.run();
    }

    pub fn getOperandAddress(self: *const CPU, mode: AddressingMode) u16 {
        return switch (mode) {
            .Immediate => self.program_counter,
            .ZeroPage => @as(u16, self.memRead(self.program_counter)),
            .Absolute => self.memReadU16(self.program_counter),
            .ZeroPage_X => {
                const pos = self.memRead(self.program_counter);
                return @as(u16, pos +% self.register_x);
            },
            .ZeroPage_Y => {
                const pos = self.memRead(self.program_counter);
                return @as(u16, pos +% self.register_y);
            },
            .Absolute_X => {
                const base = self.memReadU16(self.program_counter);
                return base +% @as(u16, self.register_x);
            },
            .Absolute_Y => {
                const base = self.memReadU16(self.program_counter);
                return base +% @as(u16, self.register_y);
            },
            .Indirect_X => {
                const base = self.memRead(self.program_counter);
                const ptr = base +% self.register_x;
                const lo = self.memRead(@as(u16, ptr));
                const hi = self.memRead(@as(u16, ptr +% 1));
                return (@as(u16, hi) << 8) | @as(u16, lo);
            },
            .Indirect_Y => {
                const base = self.memRead(self.program_counter);
                const lo = self.memRead(@as(u16, base));
                const hi = self.memRead(@as(u16, base +% 1));
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
            const code = self.memRead(self.program_counter);
            self.program_counter += 1;
            const program_counter_state = self.program_counter;

            const opcode = self.opcodes.get(code) orelse return error.InvalidOpCode;

            switch (opcode.code) {
                0xa9, 0xa5, 0xb5, 0xad, 0xbd, 0xb9, 0xa1, 0xb1 => {
                    self.lda(opcode.mode);
                },
                0x85, 0x95, 0x8d, 0x9d, 0x99, 0x81, 0x91 => {
                    self.sta(opcode.mode);
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
    try cpu.loadAndRun(&[_]u8{ 0xa9, 0x05, 0x00 });
    try std.testing.expectEqual(@as(u8, 5), cpu.register_a);
    try std.testing.expect((cpu.status & 0b0000_0010) == 0);
    try std.testing.expect((cpu.status & 0b1000_0000) == 0);
}

test "0xa9 lda zero flag" {
    const allocator = std.heap.page_allocator;
    var cpu = try CPU.init(allocator);
    defer cpu.deinit();
    try cpu.loadAndRun(&[_]u8{ 0xa9, 0x00, 0x00 });
    try std.testing.expect((cpu.status & 0b0000_0010) == 0b10);
}

test "0xa9 lda negative flag" {
    const allocator = std.heap.page_allocator;
    var cpu = try CPU.init(allocator);
    defer cpu.deinit();
    try cpu.loadAndRun(&[_]u8{ 0xa9, 0xff, 0x00 });
    try std.testing.expect((cpu.status & 0b1000_0000) == 0b1000_0000);
}

test "LDA from memory" {
    const allocator = std.heap.page_allocator;
    var cpu = try CPU.init(allocator);
    defer cpu.deinit();
    cpu.memWrite(0x10, 0x55);

    // In Zig we need to create a slice from an array
    const program = [_]u8{ 0xa5, 0x10, 0x00 };
    try cpu.loadAndRun(&program);

    try std.testing.expectEqual(@as(u8, 0x55), cpu.register_a);
}

test "0x85 sta store a in memory" {
    const allocator = std.heap.page_allocator;
    var cpu = try CPU.init(allocator);
    defer cpu.deinit();
    try cpu.loadAndRun(&[_]u8{ 0xa9, 0x05, 0x85, 0x10, 0x00 });
    try std.testing.expectEqual(@as(u8, 0x05), cpu.memRead(0x10));
}

test "0xaa tax move a to x" {
    const allocator = std.heap.page_allocator;
    var cpu = try CPU.init(allocator);
    defer cpu.deinit();
    try cpu.loadAndRun(&[_]u8{ 0xa9, 0x0A, 0xaa, 0x00 });
    try std.testing.expectEqual(@as(u8, 10), cpu.register_x);
}

test "5 ops working together" {
    const allocator = std.heap.page_allocator;
    var cpu = try CPU.init(allocator);
    defer cpu.deinit();
    try cpu.loadAndRun(&[_]u8{ 0xa9, 0xc0, 0xaa, 0xe8, 0x00 });
    try std.testing.expectEqual(@as(u8, 0xc1), cpu.register_x);
}

test "inx overflow" {
    const allocator = std.heap.page_allocator;
    var cpu = try CPU.init(allocator);
    defer cpu.deinit();
    try cpu.loadAndRun(&[_]u8{ 0xa9, 0xff, 0xaa, 0xe8, 0xe8, 0x00 });
    try std.testing.expectEqual(@as(u8, 1), cpu.register_x);
}
